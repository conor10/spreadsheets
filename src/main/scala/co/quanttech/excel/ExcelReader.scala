package co.quanttech.excel

import java.io.FileInputStream

import org.apache.poi.hssf.util.AreaReference
import org.apache.poi.ss.usermodel._
import org.apache.poi.xssf.usermodel.XSSFWorkbook

import scala.collection.Iterator

/**
 * Excel Spreadsheet reader.
 */
object ExcelReader {

  def loadSheet(fileName: String): ResultSet =
    loadSheet(fileName, 0, 0, 0)

  def loadSheet(fileName: String, sheetIdx: Int, startRowIdx: Int, startColIdx: Int): ResultSet = {
    using(loadWorkbook(fileName)) {
      workbook =>
        val evaluator = workbook.getCreationHelper().createFormulaEvaluator()

        val sheet = workbook.getSheetAt(sheetIdx)

        if (sheet.getPhysicalNumberOfRows > 0) {
          val endRowIdx = sheet.getLastRowNum + 1
          val endColIdx = sheet.getRow(startRowIdx).getLastCellNum
          val ref = TableRef(evaluator, startRowIdx, endRowIdx, startColIdx, endColIdx)
          extract(sheet, ref)
        }
        else ResultSet.empty
    }
  }

  def loadNamedRange(fileName: String, namedRange: String): ResultSet = {
    using(loadWorkbook(fileName)) {
      workbook =>
        val evaluator = workbook.getCreationHelper().createFormulaEvaluator()

        val name = workbook.getName(namedRange)
        if (name == null)
          throw new IllegalArgumentException(s"Named range: $namedRange does not exist")

        val areaReference = new AreaReference(name.getRefersToFormula)
        val firstCell = areaReference.getFirstCell
        val lastCell = areaReference.getLastCell

        val sheet = workbook.getSheet(firstCell.getSheetName)

        // Header rows will be one row above named range
        // As named ranges are typically used as lookup tables, hence the headers are not included
        val tableRef = TableRef(evaluator,
          firstCell.getRow - 1, lastCell.getRow + 1, firstCell.getCol, lastCell.getCol + 1)

        extract(sheet, tableRef)
    }
  }

  def loadWorkbook(fileName: String): Workbook = {
    val inputStream = new FileInputStream(fileName)
    new XSSFWorkbook(inputStream)
  }

  def extract(sheet: Sheet, ref: TableRef): ResultSet = {
    val headers = loadHeaders(sheet, ref)
    val data = loadValues(sheet, ref.copy(startRowIdx = ref.startRowIdx + 1))

    ResultSet(headers, data)
  }

  def loadHeaders(sheet: Sheet, ref: TableRef): Map[String, Int] = {
    val numMergedRegions = sheet.getNumMergedRegions
    val mergedRegions = for (i <- 0 until numMergedRegions) yield sheet.getMergedRegion(i)

    def getMergedCellValue(cell: Cell): Option[Any] = {
      val cellRange = mergedRegions.find(_.isInRange(cell.getRowIndex, cell.getColumnIndex))

      if (cellRange.isEmpty) read(cell, ref.evaluator)
      else {
        val range = cellRange.get
        val cells = for {
          rowIdx <- range.getFirstRow to range.getLastRow
          row = sheet.getRow(rowIdx)
          cellIdx <- range.getFirstColumn to range.getLastColumn
          cell = row.getCell(cellIdx)
        } yield read(cell, ref.evaluator)
        cells.flatten.headOption
      }
    }

    def processRow(rowIdx: Int): IndexedSeq[Option[Any]] = {
      val row = sheet.getRow(rowIdx)

      for (cellIdx <- ref.startColIdx until ref.endColIdx)
      yield getMergedCellValue(row.getCell(cellIdx, Row.CREATE_NULL_AS_BLANK))
    }

    def getHeaderString(headers: IndexedSeq[Option[Any]]): IndexedSeq[(String, Int)] =
      for ((entry, idx) <- headers.zipWithIndex)
      yield (entry.getOrElse("Unknown-" + idx).toString, idx)

    def getDualHeaderString(headers: IndexedSeq[(Option[Any], Option[Any])]):
      IndexedSeq[(String, Int)] =
      for (((entry1, entry2), idx) <- headers.zipWithIndex) yield ({
        val prefix = if (entry1.isDefined) entry1.get.toString() + " " else ""
        prefix + entry2.getOrElse("Unknown-" + idx).toString
      }, idx)

    val headers = processRow(ref.startRowIdx)
    val containsDuplicates = headers.distinct.size != headers.size

    val upperHeaderRowIdx = ref.startRowIdx - 1
    if (containsDuplicates && upperHeaderRowIdx >= sheet.getFirstRowNum) {
      val upperHeaders = processRow(ref.startRowIdx - 1)
      getDualHeaderString(upperHeaders.zip(headers)).toMap
    }
    else getHeaderString(headers).toMap
  }

  def loadValues(sheet: Sheet, ref: TableRef): IndexedSeq[IndexedSeq[Option[Any]]] = {
    for (rowIdx <- ref.startRowIdx until ref.endRowIdx;
         row = sheet.getRow(rowIdx))
    yield {
      for (cellIdx <- ref.startColIdx until ref.endColIdx;
           cell = row.getCell(cellIdx, Row.RETURN_NULL_AND_BLANK))
      yield read(cell, ref.evaluator)
    }
  }

  private def read(cell: Cell, evaluator: FormulaEvaluator): Option[Any] = {
    if (cell == null) None
    else Option(cell.getCellType match {
      case Cell.CELL_TYPE_STRING => cell.getStringCellValue
      case Cell.CELL_TYPE_NUMERIC =>
        if (DateUtil.isCellDateFormatted(cell)) cell.getDateCellValue
        else cell.getNumericCellValue
      case Cell.CELL_TYPE_BOOLEAN => cell.getBooleanCellValue
      case Cell.CELL_TYPE_FORMULA => read(evaluator.evaluate(cell), evaluator)
      case Cell.CELL_TYPE_BLANK => null
      case Cell.CELL_TYPE_ERROR => null
    })
  }

  private def read(cell: CellValue, evaluator: FormulaEvaluator): Any = {
    cell.getCellType match {
      case Cell.CELL_TYPE_STRING => cell.getStringValue
      case Cell.CELL_TYPE_NUMERIC => cell.getNumberValue
      case Cell.CELL_TYPE_BOOLEAN => cell.getBooleanValue
      case Cell.CELL_TYPE_FORMULA => // Cannot be returned
      case Cell.CELL_TYPE_BLANK => null
      case Cell.CELL_TYPE_ERROR => null
    }
  }

  // Lifted from p386 of the Scala Cookbook
  def using[A <: { def close(): Unit }, B](resource: A)(f: A => B): B = {
    try {
      f(resource)
    }
    finally {
      resource.close()
    }
  }
}


case class TableRef(evaluator: FormulaEvaluator, startRowIdx: Int, endRowIdx: Int,
                    startColIdx: Int, endColIdx: Int)


case class ResultSet(headers: Map[String, Int], values: IndexedSeq[IndexedSeq[Option[Any]]])
  extends Iterable[Entry] {

  override def iterator: Iterator[Entry] =
    values.iterator.map(entry => Entry(headers, entry))

  def apply(header: String): IndexedSeq[Option[Any]] = {
    if (headers.contains(header)) values.map(_(headers(header)))
    else IndexedSeq[Option[Any]]()
  }

  def apply(idx: Int): Entry = Entry(headers, values(idx))
}

object ResultSet {
  val empty = ResultSet(Map.empty, IndexedSeq(IndexedSeq(None)))
}


case class Entry(private val headers: Map[String, Int], values: IndexedSeq[Option[Any]])
  extends Iterable[Option[Any]] {

  override def iterator = values.iterator

  def apply(header: String): Option[Any] = {
    if (headers.contains(header)) values(headers(header))
    else None
  }

  def apply(idx: Int): Option[Any] = {
    values(idx)
  }

  override def toString() = headers.map {
    case (h, i) => h + " -> " + values(i).toString
  }.toString
}