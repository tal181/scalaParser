package scala
import SegmentType._
class Line (val dateC: String, val uidC: String, val segment_typeC: SegmentType, val segmentC: Int) {
  var date: String = dateC
  var uid: String = uidC
  var segment_type: SegmentType = segment_typeC
  var segment: Int = segmentC

  override def toString(): String = "date: " + dateC + " uid:" +uid + " segment_type : " + segment_type.toString + " segment: " +segment

}


