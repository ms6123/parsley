package parsley.internal.machine.jit.codegen

import java.io.File
import java.lang.reflect.{Field, Method, Modifier}
import java.nio.file.{Files, Paths}
import java.util.Collections

import scala.collection.mutable
import scala.reflect.ClassTag

import ClassGenContext.Constants.*
import ClassGenContext.objectPools
import parsley.internal.machine.jit.utils.DceClassAdapter

import org.objectweb.asm.*
import org.objectweb.asm.util.CheckClassAdapter

class ClassGenContext {
    private val classLoader = new OpenClassLoader(getClass.getClassLoader)

    def newClass(access: Int, name: String, superName: String = "java/lang/Object", interfaces: Seq[String] = Seq.empty)
                (builder: ClassGenVisitor => Unit): Class[?] = {
        val writer = new ClassWriter(ClassWriter.COMPUTE_FRAMES)
        val visitor = new ClassGenVisitor(new CheckClassAdapter(new DceClassAdapter(writer)), name)
        visitor.visit(Opcodes.V1_8, access, name, null, superName, interfaces.toArray)
        builder(visitor)
        visitor.visitEnd()
        val bytes = writer.toByteArray
        if (SHOULD_DUMP_CLASSES) {
            val file = Paths.get("jit-classes", name.replace('/', File.separatorChar) + ".class")
            Files.createDirectories(file.getParent)
            Files.write(file, bytes)
        }
        val clazz = classLoader.defineClass(name.replace('/', '.'), bytes)
        objectPools.put(clazz, visitor.objectPool.toArray)
        clazz
    }

    class ClassGenVisitor(delegate: ClassVisitor, private val className: String) extends ClassVisitor(Opcodes.ASM9, delegate) {
        private[ClassGenContext] val existingObjects = mutable.Map.empty[(IdentityBox, Class[?]), Int]
        private[ClassGenContext] val objectPool = mutable.ArrayBuffer.empty[AnyRef]
        private[ClassGenContext] val objectTypes = mutable.ArrayBuffer.empty[Class[?]]

        override def visitMethod(access: Int, name: String, desc: String, signature: String, exceptions: Array[String]): MethodGenVisitor = {
            new MethodGenVisitor(super.visitMethod(access, name, desc, signature, exceptions), className, { case (obj, clazz) =>
                val index = existingObjects.getOrElseUpdate((new IdentityBox(obj), clazz), {
                    objectPool += obj
                    objectTypes += clazz
                    objectPool.length - 1
                })

                clazz.getSimpleName + index
            })
        }

        override def visitEnd(): Unit = {
            val clinit = visitMethod(Opcodes.ACC_STATIC, "<clinit>", "()V", null, null)
            for (((obj, clazz), index) <- objectPool.zip(objectTypes).zipWithIndex) {
                visitField(Opcodes.ACC_PRIVATE | Opcodes.ACC_STATIC | Opcodes.ACC_FINAL, clazz.getSimpleName + index, Type.getDescriptor(clazz), null, null).visitEnd()

                clinit.visitLdcInsn(Type.getObjectType(className))
                clinit.visitLdcInsn(obj.toString)
                clinit.loadInt(index)
                clinit.callMethod(Members.JitRuntime.GET_OBJECT)
                clinit.visitTypeInsn(Opcodes.CHECKCAST, Type.getInternalName(clazz))

                clinit.visitFieldInsn(Opcodes.PUTSTATIC, className, clazz.getSimpleName + index, Type.getDescriptor(clazz))
            }
            clinit.visitInsn(Opcodes.RETURN)
            clinit.visitEnd()
        }
    }

    class MethodGenVisitor(delegate: MethodVisitor, private val className: String, private val registerObject: (AnyRef, Class[?]) => String) extends MethodVisitor(Opcodes.ASM9, delegate) {
        visitCode()

        def loadObject[T <: AnyRef](obj: T)(implicit tag: ClassTag[T]): Unit = loadAny(obj, tag.runtimeClass)

        def loadAny(obj: AnyRef, cls: Class[?]): Unit =
            if (cls.isPrimitive) {
                cls match {
                    case java.lang.Boolean.TYPE => visitInsn(if (obj.asInstanceOf[Boolean]) Opcodes.ICONST_1 else Opcodes.ICONST_0)
                    case java.lang.Character.TYPE => loadInt(obj.asInstanceOf[Char])
                }
            } else if (obj eq null) {
                visitInsn(Opcodes.ACONST_NULL)
            } else if (isScalaObject(obj)) {
                visitFieldInsn(Opcodes.GETSTATIC, Type.getInternalName(obj.getClass), "MODULE$", Type.getDescriptor(obj.getClass))
            } else {
                visitFieldInsn(Opcodes.GETSTATIC, className, registerObject(obj, cls), Type.getDescriptor(cls))
            }

        def loadInt(i: Int): Unit = {
            i match {
                case _ if (-1 to 5).contains(i) => visitInsn(Opcodes.ICONST_0 + i)
                case _ if (Byte.MinValue to Byte.MaxValue).contains(i) => visitIntInsn(Opcodes.BIPUSH, i)
                case _ if (Short.MinValue to Short.MaxValue).contains(i) => visitIntInsn(Opcodes.SIPUSH, i)
                case _ => visitLdcInsn(i)
            }
        }

        def callMethod(method: Method): Unit = {
            val modifiers = method.getModifiers
            val isInterface = method.getDeclaringClass.isInterface
            val opcode = if (Modifier.isStatic(modifiers)) {
                Opcodes.INVOKESTATIC
            } else if (Modifier.isPrivate(modifiers)) {
                Opcodes.INVOKESPECIAL
            } else if (isInterface) {
                Opcodes.INVOKEINTERFACE
            } else {
                Opcodes.INVOKEVIRTUAL
            }
            visitMethodInsn(opcode, Type.getInternalName(method.getDeclaringClass), method.getName, Type.getMethodDescriptor(method), isInterface)
        }

        def getField(field: Field): Unit = {
            val opcode = if (Modifier.isStatic(field.getModifiers)) Opcodes.GETSTATIC else Opcodes.GETFIELD
            visitFieldInsn(opcode, Type.getInternalName(field.getDeclaringClass), field.getName, Type.getDescriptor(field.getType))
        }

        def putField(field: Field): Unit = {
            val opcode = if (Modifier.isStatic(field.getModifiers)) Opcodes.PUTSTATIC else Opcodes.PUTFIELD
            visitFieldInsn(opcode, Type.getInternalName(field.getDeclaringClass), field.getName, Type.getDescriptor(field.getType))
        }

        override def visitEnd(): Unit = {
            visitMaxs(0, 0)
            super.visitEnd()
        }

        private def isScalaObject(obj: AnyRef): Boolean =
            try {
                obj.getClass.getField("MODULE$")
                true
            } catch {
                case _: NoSuchFieldException => false
            }
    }
}

object ClassGenContext {
    private[jit] object Constants {
        val SHOULD_DUMP_CLASSES: Boolean = System.getProperty("parsley.jit.dump", "false").toBoolean
    }

    private val objectPools = Collections.synchronizedMap(new java.util.WeakHashMap[Class[?], Array[AnyRef]])

    def getObject(index: Int, context: Class[?]): AnyRef = {
        objectPools.get(context)(index)
    }
}

private class OpenClassLoader(parent: ClassLoader) extends ClassLoader(parent) {
    def defineClass(name: String, bytes: Array[Byte]): Class[_] = super.defineClass(name, bytes, 0, bytes.length)
}

private class IdentityBox(val value: AnyRef) {
    override def equals(obj: Any): Boolean = obj.isInstanceOf[IdentityBox] && (obj.asInstanceOf[IdentityBox].value eq value)
    override def hashCode(): Int = System.identityHashCode(value)
}