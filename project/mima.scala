package parsley.build

import com.typesafe.tools.mima.core._

object mima {
    val issueFilters = Seq(
        ProblemFilters.exclude[Problem]("parsley.internal.*"),
    )
}
