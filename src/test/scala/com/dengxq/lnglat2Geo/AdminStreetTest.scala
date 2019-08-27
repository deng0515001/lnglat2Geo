package com.dengxq.lnglat2Geo

import com.dengxq.lnglat2Geo.entity.{Admin, Location}
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class AdminStreetTest extends FlatSpec {
//  val chinaCellNode: DistrictNode = AdminArea.loadChina
//  val chinaStreetMap: StreetMap = AdminArea.loadChinaStreet
  val testSource = Array(
    //    (112.596628,34.973136), //
    //    (122.376448,29.972796), //
    //    (119.203405,39.374154), //
    (121.1572265625,23.9260130330),  // 中国台湾省南投县仁爱乡
    (112.567757,35.096176),         // 济源
    (116.9565868378,39.6513677208), // 天津市武清区河西务镇
    (100.4315185547,21.7594997307), // 中国云南省西双版纳傣族自治州勐海县勐混镇
    (85.5670166016,41.5548386631),  // 中国新疆维吾尔自治区巴音郭楞蒙古自治州库尔勒市 普惠乡
    (117.9969406128,27.7447712551), // 中国福建省南平市武夷山市 崇安街道
    (110.8520507813,34.0526594214), // 河南省三门峡市卢氏县 瓦窑沟乡下河村
    (116.4811706543,39.9255352817), // 北京市朝阳区 六里屯街道甜水园
    (116.3362348080,40.0622912084), // 北京市昌平区 回龙观地区吉晟别墅社区
    (116.3362830877,40.0594500522), // 北京市北京市昌平区 建材城西路65号
    (116.3325601816,40.0397393499), // 北京市海淀区 清河街道 小米六期
    (117.0977783203,36.5085323575), // 山东省济南市历城区
    (118.6358642578,35.8356283889), // 山东省临沂市沂水县
    (119.7853088379,36.3029520437), // 山东省潍坊市高密市柏城镇
    (119.8567199707,36.2808142593), // 山东省青岛市胶州市胶西镇
    (120.3892135620,36.2777698228), // 山东省青岛市城阳区流亭街道于家社区
    (116.3830447197,39.9467430610),  // 中国北京市北京市西城区鼓楼西大街171号
    (116.3854265213,39.9444070723)   // 北京市西城区什刹海街道鼓西社区
  )

  val testResult = Array(
    Admin.createProvincialCity("台湾省","南投县", 710008,710008, Location(0.0,0.0)),
    Admin.createProvincialCity("河南省","济源市", 419001,  419001, Location(112.602256,35.067199)),
    Admin.createDistrict("天津市", "天津城区", "武清区", 120100, 120100, 120114, Location(117.044387,39.384119)),
    Admin.createDistrict( "云南省", "西双版纳傣族自治州", "勐海县", 532800,532800, 532822, Location(100.452547, 21.957353)),
    Admin.createDistrict("新疆维吾尔自治区", "巴音郭楞蒙古自治州", "库尔勒市", 652800, 652800, 652801, Location(86.174633,41.725891)),
    Admin.createDistrict("福建省", "南平市", "武夷山市", 350700, 350700, 350782, Location(118.035309,27.756647)),
    Admin.createDistrict("河南省", "三门峡市", "卢氏县", 411200, 411200, 411224, Location(111.047858,34.054324)),
    Admin.createDistrict("北京市", "北京城区", "朝阳区", 110100, 110100, 110105, Location(116.443205,39.921506)),
    Admin.createDistrict("北京市", "北京城区", "昌平区", 110100, 110100, 110114, Location(116.231254,40.220804)),
    Admin.createDistrict("北京市", "北京城区", "昌平区", 110100, 110100, 110114, Location(116.231254,40.220804)),
    Admin.createDistrict("北京市", "北京城区", "海淀区", 110100, 110100, 110108, Location(116.298262,39.95993)),
    Admin.createDistrict("山东省", "济南市", "历城区", 370100, 370100, 370112, Location(117.06523,36.680259)),
    Admin.createDistrict("山东省", "临沂市", "沂水县", 371300, 371300, 371323, Location(118.627917,35.79045)),
    Admin.createDistrict( "山东省", "潍坊市", "高密市", 370700, 370700, 370785, Location(119.755597,36.382594)),
    Admin.createDistrict("山东省", "青岛市", "胶州市", 370200, 370200, 370281, Location(120.033382,36.26468)),
    Admin.createDistrict( "山东省", "青岛市", "城阳区", 370200, 370200, 370214, Location(120.396256,36.307559))
  )

  "AdminStreet.nearestStreet in scala " should "work" in {
//    for(i <- 0 until Math.min(testSource.length,testResult.length) - 1) {
//      val lonlat = testSource(i)
//      val admin = AdminArea.nearestStreet(lonlat, CoordinateSystem.GCJ02, chinaCellNode, chinaStreetMap)
//      println(admin)
//    }
  }

    "AdminStreet.nearestStreet in scala " should "success - qps > 14w" in {

//      val range = 100000
//      val total = range * testSource.length
//      var n = 0
//      val start = new Date().getTime
//      for(x <- 1 to range) {
//
//        for(i <- testSource.indices) {
//          val lonlat = testSource(i)
//
//          val _ = AdminArea.nearestStreet(lonlat, CoordinateSystem.GCJ02, chinaCellNode, chinaStreetMap)
//          n = n + 1
//          if(n%10000 == 0) {
//            logger.info(n+" case done")
//            n = 0
//          }
//        }
//      }
//      if(n > 0) logger.info(n+" case done")
//
//      val end = new Date().getTime
//      val elapsed = end - start
//      val qps = total / (elapsed/1000)
//      logger.info("done elapsed:" + elapsed + "ms, qps:" + qps + "/s")
//      assert(qps > 140000, "qps need > 14w")
    }
  //
  //  "AdminStreet in scala " should " success" in {
  //    for(i <- 0 until Math.min(testSource.length,testResult.length) - 1) {
  //      val lonlat = testSource(i)
  //      val startLoad = new Date()
  //      val admin = AdminArea.determineStreet(lonlat, CoordinateSystem.GCJ02)
  //      val endLoad = new Date()
  //      logger.info(admin.toJson+"\tCalc time:"+ (endLoad.getTime - startLoad.getTime))
  //      val shouldAdmin = testResult(i)
  ////      assert(shouldAdmin.toJson == admin.toJson)
  //    }
  //    logger.info("done")
  //
  //  }
  //
  //
  //  "AdminStreet in spark " should " success" in {
  //
  //    val a= sparkContext.parallelize(Array(chinaCellNode))
  //    a.foreach(r=>{
  //      println(r)
  //    })
  //
  //    val china = sparkContext.broadcast(chinaCellNode)
  //    val chinaStreets = sparkContext.broadcast(chinaStreetMap)
  //    sparkContext.parallelize(testSource).repartition(3).map(lnglat=>{
  //      val admin = AdminArea.determineStreet(lnglat, CoordinateSystem.GCJ02)(china.value,chinaStreets.value)
  //      admin.toJson
  //    }).collect().foreach(println)
  //
  //  }
}
