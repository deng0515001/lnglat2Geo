package com.dengxq.lnglat2Geo

import com.dengxq.lnglat2Geo.entity.{Admin, CoordinateSystem, Location}
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class AdminTest extends FlatSpec {
//  val chinaCellNode: DistrictNode = AdminArea.loadChina

  val testSource = Array(
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
    (116.3325601816,40.0397393499), // 北京市海淀区 清河街道
    (117.0977783203,36.5085323575), // 山东省济南市历城区
    (118.6358642578,35.8356283889), // 山东省临沂市沂水县
    (119.7853088379,36.3029520437), // 山东省潍坊市高密市柏城镇
    (119.8567199707,36.2808142593), // 山东省青岛市胶州市胶西镇
    (120.3892135620,36.2777698228), // 山东省青岛市城阳区流亭街道于家社区
    (120.152983,36.119759), // 海外
    (98.774694,23.706633) // 海外
//    (116.3830447197,39.9467430610),  // 中国北京市北京市西城区鼓楼西大街171号
//    (116.3854265213,39.9444070723)   // 北京市西城区什刹海街道鼓西社区
    // (116.3363742828, 40.0300297342) // 北京市海淀区清河街道 五彩城购物中心东区
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
    Admin.createDistrict( "山东省", "青岛市", "城阳区", 370200, 370200, 370214, Location(120.396256,36.307559)),
    Admin.createDistrict("海外", "海外", "海外", -1, -1, -1, Location(120.396256,36.307559)),
    Admin.createDistrict("海外", "海外", "海外", -1, -1, -1, Location(120.396256,36.307559))
  )

  "AdminArea.determineAdmin in scala " should "qps > 14w" in {

//    val range = 10000
//    val total = range * testSource.length
//    var n = 0
//    val start = new Date().getTime
//    for(x <- 1 to range) {
//
//      for(i <- testSource.indices) {
//        val lonlat = testSource(i)
//
//        val _ = AdminArea.determineAdmin(lonlat, CoordinateSystem.GCJ02, chinaCellNode)
//        n = n + 1
//        if(n%10000 == 0) {
//          logger.info(n+" case done")
//          n = 0
//        }
//      }
//    }
//    if(n > 0) logger.info(n+" case done")
//
//    val end = new Date().getTime
//    val elapsed = end - start
//    val qps = total / (elapsed/1000)
//    logger.info("done elapsed:" + elapsed + "ms, qps:" + qps + "/s")
//    assert(qps > 140000, "qps need > 14w")
  }

  "AdminArea.determineAdmin in spark " should "work" in {

//    val a= sparkContext.parallelize(Array(chinaCellNode))
//    a.foreach(r=>{
//      println(r)
//    })
//
//    val china = sparkContext.broadcast(chinaCellNode)
//    sparkContext.parallelize(testSource).repartition(3).map(lnglat=>{
//      val aroundAreas = AdminArea.determineAdmin(lnglat, CoordinateSystem.GCJ02, china.value)
//      aroundAreas.toJson
//    }).collect().foreach(println)
  }



  "AdminArea.determineAdmin in scala" should "work" in {

    val start3 = System.currentTimeMillis()
    GeoTrans.init()
    println("cost1 = " + (System.currentTimeMillis() - start3))

    val start1 = System.currentTimeMillis()
    println(GeoTrans.determineAdmin(120.152983,36.119759, CoordinateSystem.GCJ02))
    println("cost2 = " + (System.currentTimeMillis() - start1))

//    val str = "115.068863,30.203944;112.905879,28.281898;122.956308,41.098388; 114.32782,34.787914;113.830148,34.638862;113.078327,28.161037;116.415704,39.953465;120.551632,31.283096;106.114479, 30.82612;111.658452,23.151697;115.642757,34.412346;112.226943,30.365528;117.235243,32.755445;119.652622,29.067311;111.418815,27.676285;113.117786,36.183096; 96.123861,16.835776;116.659674,39.516241;106.068781,30.765933;121.512104,29.897897"

    val str = "112.989382,28.147062;109.046404,35.042294;106.559531,29.607832;119.481842,33.652686;116.525612,39.824004;109.090599,35.080281;113.508112,37.892087;123.417829,41.791227;120.517459,30.459049;113.865295,35.290525;110.290043,20.015764;108.934191,34.294362;117.183897,34.264914;126.587992,45.757869;115.859063,28.695778;106.771075,26.584885; 108.92224,34.233088;113.809742,23.067213;118.778811,32.089465;113.715261,35.2587"
    str.split(";").foreach(s => {
      val parts = s.split(",")
      val ss = GeoTrans.determineAdmin(parts(0).toDouble, parts(1).toDouble, CoordinateSystem.WGS84)
      println(ss)
    })
//    println(AdminArea.determineAdmin((119.52320949527594,34.62947031340603), CoordinateSystem.WGS84, chinaCellNode))
//    println(AdminArea.determineAdmin((119.55377645685893,34.639698133907125), CoordinateSystem.WGS84, chinaCellNode))
//    println(AdminArea.determineAdmin((111.739463,40.842317), CoordinateSystem.GCJ02, chinaCellNode))
//    println(AdminArea.determineAdmin((111.731608,40.847438), CoordinateSystem.GCJ02, chinaCellNode))

     testSource.zip(testResult).foreach( {
      case (lnglat, adminR) =>
        val admin = GeoTrans.determineAdmin(lnglat._1, lnglat._2,  CoordinateSystem.GCJ02)
        println(admin)
        println(adminR)

        assert(admin.province == adminR.province)
        assert(admin.city == adminR.city)
        assert(admin.cityCode == adminR.cityCode)
        assert(admin.district == adminR.district)
        assert(admin.districtCode == adminR.districtCode)
      })

//    List().map(s => {
//      val start = new S2CellId(s).toLatLng
//      (start.lngDegrees(), start.latDegrees())
//    }).foreach(println)

//    val start = System.currentTimeMillis()

//    List((114.905215,4.887684), (101.049631,41.944256), (115.132069,37.300413), (99.15034016844125,22.16272593989059)).foreach(s => {
//      val admin = GeoSpatial.determineAdmin(s._1, s._2, true, coordSys = CoordinateSystem.WGS84)
//      println(admin, s)
//    })

//    val sss = Source.fromFile("/home/dengxq/Downloads/loc.csv").getLines()
//      .map(s => {
//      val parts = s.split(",")
//      val admin = GeoSpatial.determineAdminCode(parts(2).toDouble, parts(3).toDouble)
//      (admin, parts(2), parts(3))
//    })
//      .toList
//    println("cost3 = " + (System.currentTimeMillis() - start))
//    println(sss.size)
//    println(ss.maxBy(_._1)) //&& s._2 != "530827"
//    println(ss.map(_._1).sum / ss.size)
//    println(ss.map(_._1).sum)
//    println(sss.count(s => s._1 != -1))
  }


}
