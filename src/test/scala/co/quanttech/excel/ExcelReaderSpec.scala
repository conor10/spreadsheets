package co.quanttech.excel

import java.io.FileNotFoundException
import java.util.Date

import org.scalatest.{Matchers, FlatSpec}

/**
 * ExcelReader specification tests.
 */
class ExcelReaderSpec extends FlatSpec with Matchers {

  val FileName = "src/test/resources/Workbook.xlsx"

  def verify(rs: ResultSet, colCount: Int, rowCount: Int) = {
    assert(colCount === rs.headers.size)
    assert(rowCount === rs.values.size)
    assert(colCount === rs.values.head.size)

    assert(rs.headers.forall(_._1.length > 0))
    assert(rs.headers.map(_._1).toSet.size === rs.headers.size)
    assert(rs.headers.forall(x => x._2 >= 0 && x._2 < rs.headers.size))

    assert(rs.values.forall(_.size == colCount))
  }

  def verifyEntries(rs: ResultSet) = {
    assert(rs.values.forall(row => row.forall(_ match {
      case Some(x: String) => x.length > 0
      case Some(x: Double) => x != 0.0
      case Some(x: Date) => x.getTime > 0
      case Some(_) => false
      case None => false
    })))
  }

  behavior of "ExcelReader"

  it should "process a zero indexed table" in {
    val rs = ExcelReader.loadSheet(FileName, 0, 0, 0)
    verify(rs, 3, 9)
    verifyEntries(rs)
  }

  it should "process a non-zero indexed table" in {
    val rs = ExcelReader.loadSheet(FileName, 1, 2, 1)
    verify(rs, 3, 9)
    verifyEntries(rs)
  }

  it should "process formulas" in {
    val rs = ExcelReader.loadSheet(FileName, 2, 0, 0)
    verify(rs, 9, 10)
    verifyEntries(rs)
  }

  it should "process tables with empty cells" in {
    val rs = ExcelReader.loadSheet(FileName, 3, 0, 0)
    verify(rs, 3, 10)
  }

  it should "process the named range named_range" in {
    val rs = ExcelReader.loadNamedRange(FileName, "named_range")
    verify(rs, 3, 9)
    verifyEntries(rs)
  }

  it should "process merged header cells" in {
    val rs = ExcelReader.loadSheet(FileName, 5, 1, 0)
    verify(rs, 3, 9)
    verifyEntries(rs)
  }

  it should "process repeating headers" in {
    val rs = ExcelReader.loadSheet(FileName, 6, 1, 0)
    verify(rs, 2, 9)
    verifyEntries(rs)
    assert(rs.headers.contains("Left Text"))
    assert(rs.headers.contains("Right Text"))
  }

  it should "process merged and repeating headers" in {
    val rs = ExcelReader.loadSheet(FileName, 7, 1, 0)
    verify(rs, 6, 9)
    verifyEntries(rs)
    assert(rs.headers.contains("Left Date"))
    assert(rs.headers.contains("Left Text"))
    assert(rs.headers.contains("Left Amount"))
    assert(rs.headers.contains("Right Date"))
    assert(rs.headers.contains("Right Text"))
    assert(rs.headers.contains("Right Amount"))
  }

  it should "ignore upper headers if cells are not repeating" in {
    val rs = ExcelReader.loadSheet(FileName, 8, 1, 0)
    verify(rs, 2, 9)
    verifyEntries(rs)
    assert(rs.headers.contains("Text1"))
    assert(rs.headers.contains("Text2"))
  }

  it should "throw an FileNotFoundException if an invalid filename is provided" in {
    an [FileNotFoundException] should be thrownBy ExcelReader.loadSheet("invalid")
  }

  it should "throw an IllegalArgumentException if an invalid named range is provided" in {
    an [IllegalArgumentException] should be thrownBy ExcelReader.loadNamedRange(FileName, "invalid")
  }

  it should "provide an empty dataset if the sheet is blank" in {
    assert(ExcelReader.loadSheet(FileName, 9, 0, 0) === ResultSet.empty)
  }
}
