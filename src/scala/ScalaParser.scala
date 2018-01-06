import ScalaParser.buildMap

import scala.io.Source
import SegmentType._
import scala.collection.mutable.ArrayBuffer
object ScalaParser {

  def main(args: Array[String]): Unit = {

    //CAMPAIGN->LIST(STAGES),MAP(TACTICS->LIST(IMPRESSIONS))
    val metadata = Map(100 -> (List(200, 201), Map(400 -> List(300, 301))),
      101 -> (List(201), Map(401 -> List(302), 402 -> List(303))),
      102 -> (List(202), Map(400 -> List(300, 302))))

    //parse file
    var linesArray = parseFile()

    //create impressions -> tactics
    var impressionTacticMapping = createImpressionsTacticsMap(metadata)

    //create stages -> campaigns
    var stageCampaignMapping = createStagesCampaignMap(metadata)

    //createTacticsCampaignsMap
    var tacticsCampaignsMap = createTacticsCampaignsMap(metadata)

    val resultMap = buildResMap(linesArray,metadata, impressionTacticMapping, stageCampaignMapping, tacticsCampaignsMap)

    resultMap.foreach(x => {
      print(x)
    })

  }

  def createImpressionsTacticsMap(map: Map[Int, (List[Int], Map[Int, List[Int]])]): collection.mutable.Map[Int, collection.mutable.Set[Int]] = {
    val returnedMap = collection.mutable.Map[Int, collection.mutable.Set[Int]]()
    map.foreach(x => {
      val (stages, tactics) = x._2

      tactics.foreach(tacticMapping => {

        val tactic = tacticMapping._1
        val impressions = tacticMapping._2
        impressions.foreach(impression => {
          returnedMap.get(impression) match {
            case Some(values: collection.mutable.Set[Int]) => returnedMap.update({
              impression
            }, values += tactic)
            case None => returnedMap.update({impression}, collection.mutable.Set( tactic))
          }

        })

      })
    })

    return returnedMap;
  }

  def createTacticsCampaignsMap(map: Map[Int, (List[Int], Map[Int, List[Int]])]): collection.mutable.Map[Int, collection.mutable.Set[Int]] = {
    val returnedMap = collection.mutable.Map[Int, collection.mutable.Set[Int]]()

    map.foreach(x => {
      val campaignCode = x._1
      val (stages, tactics) = x._2

      tactics.foreach(tactic => {
        returnedMap.get({
          tactic._1
        }) match {
          case Some(values: collection.mutable.Set[Int]) => returnedMap.update({
            tactic._1
          }, values += campaignCode)
          case None => returnedMap.update({tactic._1}, collection.mutable.Set( campaignCode))
        }
      })
    })

    return returnedMap;
  }

  def createStagesCampaignMap(map: Map[Int, (List[Int], Map[Int, List[Int]])]): collection.mutable.Map[Int, collection.mutable.Set[Int]] = {
    val returnedMap = collection.mutable.Map[Int, collection.mutable.Set[Int]]()
    map.foreach(x => {
      val campaignCode = x._1
      val (stages, tactics) = x._2

      stages.foreach(stage => {
        returnedMap.get({
          stage
        }) match {
          case Some(values: collection.mutable.Set[Int]) => returnedMap.update({
            stage
          }, values += campaignCode)
          case None => returnedMap.update({stage}, collection.mutable.Set( campaignCode))
        }
      })
    })
    return returnedMap
  }

  def buildMap(map: Map[Int, (List[Int], Map[Int, List[Int]])]):
  collection.mutable.Map[Int, collection.mutable.Map[String,collection.mutable.Map[String,(List[Int],List[Int])]]] = {

    var resultMap = collection.mutable.Map[Int, collection.mutable.Map[String,collection.mutable.Map[String,(List[Int],List[Int])]]]()

    map.keySet.foreach(key => {
      resultMap += key -> (collection.mutable.Map())
    })

    return resultMap
  }


  def parseFile(): ArrayBuffer[Line] = {

    val fileStream = getClass.getResourceAsStream("main/resources/events.log")
    val lines = Source.fromInputStream(fileStream).getLines
    val linesArray = ArrayBuffer[Line]()
    lines.foreach(line => {
      val stringArray = line.split(",")
      val parsedLine = new Line(stringArray(0), stringArray(1), SegmentType.withNameWithDefault(stringArray(2)), stringArray(3).toInt)
      linesArray += parsedLine

    })
    return linesArray
  }

  def addSegemntsTactics(date:String, segment: Int,tactic : Int, values: collection.mutable.Map[String,(List[Int],List[Int])]) :
                         collection.mutable.Map[String,(List[Int],List[Int])] = {
    var returnValues = collection.mutable.Map[String,(List[Int],List[Int])]() ++= values


    returnValues.get({
      date
    }) match {
      case Some(values: (List[Int],List[Int])) => returnValues.update({
        date
      }, (values._1 ,values._2 :+ tactic))
      case None => returnValues.update({ date},  (List(tactic),List()))
    }

    return returnValues
  }

  def addSegemntsStages(date:String, segment: Int,stage : Int, values: collection.mutable.Map[String,(List[Int],List[Int])]) :
                        collection.mutable.Map[String,(List[Int],List[Int])] = {
    var returnValues = collection.mutable.Map[String,(List[Int],List[Int])]() ++= values

    returnValues.get({
      date
    }) match {
      case Some(values: (List[Int],List[Int])) => returnValues.update({
        date
      }, (values._1 :+ stage,values._2))
      case None => returnValues.update({ date},  (List(),List(stage)))
    }

    return returnValues
  }

  def buildCampaignValues(tacticOrStage: Int,line : Line, values : collection.mutable.Map[String,collection.mutable.Map[String,(List[Int],List[Int])]]) :
  collection.mutable.Map[String,collection.mutable.Map[String,(List[Int],List[Int])]] = {


    var returnValues = collection.mutable.Map[String,collection.mutable.Map[String,(List[Int],List[Int])]]() ++= values

    if (line.segment_type == SegmentType.IMPRESSION) {
      returnValues.get({
        line.uid
      }) match {
        case Some(values: collection.mutable.Map[String,(List[Int],List[Int])]) => returnValues.update({
          line.uid
        }, addSegemntsTactics(line.date,line.segment,tacticOrStage,values))
           case None => returnValues.update({ line.uid},  collection.mutable.Map(line.date -> (List(),List(tacticOrStage))))
      }
    }
    else if (line.segment_type == SegmentType.STAGE) {
      returnValues.get({
        line.uid
      }) match {
        case Some(values: collection.mutable.Map[String,(List[Int],List[Int])]) => returnValues.update({
          line.uid
        }, addSegemntsStages(line.date,line.segment,tacticOrStage,values))
         case None => returnValues.update({ line.uid},  collection.mutable.Map(line.date -> (List(tacticOrStage),List())))
      }
    }

    return returnValues;

  }
  //Map(100->Map(U3->Map(2017-12-22->List(),List(400))))
  def buildResMap(lines: ArrayBuffer[Line],
                  metadata: Map[Int, (List[Int], Map[Int, List[Int]])],
                  impressionTacticMapping: collection.mutable.Map[Int, collection.mutable.Set[Int]],
                  stageCampaignMapping: collection.mutable.Map[Int, collection.mutable.Set[Int]],
                  tacticsCampaignsMaping: collection.mutable.Map[Int, collection.mutable.Set[Int]]
                 ): collection.mutable.Map[Int, collection.mutable.Map[String,collection.mutable.Map[String,(List[Int],List[Int])]]] = {

    var builNewMap = buildMap(metadata)


    lines.foreach(ele => {
      if (ele.segment_type == SegmentType.IMPRESSION) {
        var tactics = impressionTacticMapping.get(ele.segment)

        tactics.get.foreach(tactic => {
          var campaigns = tacticsCampaignsMaping.get(tactic)
          campaigns.get.foreach(campaign => {

            builNewMap.get({
              campaign
            }) match {
              case Some(values: collection.mutable.Map[String,collection.mutable.Map[String,(List[Int],List[Int])]]) =>
                builNewMap.update({campaign}, buildCampaignValues(tactic,ele, values))
            }


          })
        })

      }
      else if (ele.segment_type == SegmentType.STAGE) {
        var stages = impressionTacticMapping.get(ele.segment)
        stages.get.foreach(stage => {
          var campaigns = stageCampaignMapping.get(stage)
          campaigns.get.foreach(campaign => {

            builNewMap.get({
              campaign
            }) match {
              case Some(values: collection.mutable.Map[String,collection.mutable.Map[String,(List[Int],List[Int])]]) =>
                builNewMap.update({campaign}, buildCampaignValues(stage,ele, values))
            }
          })

        })
      }


    })
    return builNewMap
  }
}
