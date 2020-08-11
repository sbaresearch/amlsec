package org.sba_research.utils

import java.io.{File, FileWriter}

import org.sba_research.Config

object PerformanceMeasurement {

  // cf. https://stackoverflow.com/a/9160068/5107545
  def time[R](label: Option[String] = None, block: => R): R = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    val config = Config()
    if (config.debugConfig.writePerformanceReport)
      writeElapsedTimeToFile(label, (t1 - t0), config.debugConfig.outputPathPerformanceReport)
    result
  }

  def writeElapsedTimeToFile(label: Option[String], time: Long, performanceReportFilePath: String): Unit = {
    val performanceReportFile = new File(performanceReportFilePath)
    // Create all the parent's directories, if needed
    performanceReportFile.getParentFile.mkdirs()
    // Will do nothing, if file already exists
    performanceReportFile.createNewFile
    val fw = new FileWriter(performanceReportFile, true)
    try {
      label.foreach(l => fw.write(s"$l:\n"))
      fw.write(s"Elapsed time: $time ns\n")
    }
    finally fw.close()
  }

}
