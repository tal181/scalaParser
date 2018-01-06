package scala

 object SegmentType extends Enumeration {
        type SegmentType = Value
        val IMPRESSION, STAGE,UNKNOWN = Value

   def withNameWithDefault(name: String): Value =
     values.find(_.toString.toLowerCase == name.toLowerCase()).getOrElse(UNKNOWN)
 }