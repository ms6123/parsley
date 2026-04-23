package parsley.internal.machine.jit

import java.io.File
import java.lang.invoke.{MethodHandles, MethodType}
import java.nio.file.{Files, Paths}

import scala.collection.mutable
import scala.reflect.ClassTag

import parsley.internal.machine.jit.ClassGenContext.objectPools

import org.objectweb.asm.*

private val SHOULD_DUMP_CLASSES = System.getProperty("parsley.jit.dump", "false").toBoolean
private val JIT_RUNTIME = Type.getInternalName(classOf[JitRuntime])
private val GET_OBJECT = classOf[JitRuntime].getMethod("getObject", classOf[MethodHandles.Lookup], classOf[String], classOf[MethodType], classOf[Int])

class ClassGenContext {
    private val classLoader = OpenClassLoader(getClass.getClassLoader)

    def newClass(access: Int, name: String, superName: String = "java/lang/Object", interfaces: Seq[String] = Seq.empty)
                (builder: ClassGenVisitor => Unit): Class[?] = {
        val writer = ClassWriter(ClassWriter.COMPUTE_FRAMES)
        val visitor = ClassGenVisitor(writer)
        visitor.visit(Opcodes.V1_8, access, name, null, superName, interfaces.toArray)
        builder(visitor)
        val bytes = writer.toByteArray
        if (SHOULD_DUMP_CLASSES) {
            val file = Paths.get("jit-classes", name.replace('/', File.separatorChar) + ".class")
            Files.createDirectories(file.getParent)
            Files.write(file, bytes)
        }
        val clazz = classLoader.defineClass(name.replace('/', '.'), bytes)
        objectPools(clazz) = visitor.objectPool.result()
        clazz
    }

    class ClassGenVisitor(delegate: ClassVisitor) extends ClassVisitor(Opcodes.ASM9, delegate) {
        private[ClassGenContext] val objectPool = mutable.ArrayBuilder.make[AnyRef]

        override def visitMethod(access: Int, name: String, desc: String, signature: String, exceptions: Array[String]): MethodGenVisitor = {
            MethodGenVisitor(super.visitMethod(access, name, desc, signature, exceptions), objectPool)
        }
    }

    class MethodGenVisitor(delegate: MethodVisitor, private val objectPool: mutable.ArrayBuilder[AnyRef]) extends MethodVisitor(Opcodes.ASM9, delegate) {
        visitCode()

        def loadObject[T <: AnyRef](obj: T)(using tag: ClassTag[T]): Unit = {
            objectPool += obj
            visitInvokeDynamicInsn(obj.getClass.getSimpleName, "()" + Type.getDescriptor(tag.runtimeClass), Handle(Opcodes.H_INVOKESTATIC, JIT_RUNTIME, GET_OBJECT.getName, Type.getMethodDescriptor(GET_OBJECT), false), objectPool.length - 1)
        }

        def loadInt(i: Int): Unit = {
            i match {
                case _ if (-1 to 5).contains(i) => visitInsn(Opcodes.ICONST_0 + i)
                case _ if (Byte.MinValue to Byte.MaxValue).contains(i) => visitIntInsn(Opcodes.BIPUSH, i)
                case _ if (Short.MinValue to Short.MaxValue).contains(i) => visitIntInsn(Opcodes.SIPUSH, i)
                case _ => visitLdcInsn(i)
            }
        }

        override def visitEnd(): Unit = {
            visitMaxs(0, 0)
            super.visitEnd()
        }
    }
}

object ClassGenContext {
    private val objectPools = mutable.WeakHashMap.empty[Class[?], Array[AnyRef]]

    def getObject(index: Int, context: Class[?]): AnyRef = {
        objectPools(context)(index)
    }
}

private class OpenClassLoader(parent: ClassLoader) extends ClassLoader(parent) {
    def defineClass(name: String, bytes: Array[Byte]): Class[_] = super.defineClass(name, bytes, 0, bytes.length)
}