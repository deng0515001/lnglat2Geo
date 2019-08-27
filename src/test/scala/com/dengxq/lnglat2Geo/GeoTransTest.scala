package com.dengxq.lnglat2Geo

import com.dengxq.lnglat2Geo.GeoTransImpl.min_level
import com.dengxq.lnglat2Geo.build.AdminDataProvider.DistrictLoader
import com.dengxq.lnglat2Geo.entity.{Admin, Location}
import com.dengxq.lnglat2Geo.utils.S2Utils
import com.dengxq.lnglat2Geo.utils.S2Utils.{childrenCellId, getLevel}
import com.google.common.geometry._
import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class GeoTransTest extends FlatSpec {

  val testSource = Array(
    //    (112.596628,34.973136), //
    //    (122.376448,29.972796), //
    //    (119.203405,39.374154), //
    (121.1572265625, 23.9260130330), // 中国台湾省南投县仁爱乡
    (112.567757, 35.096176), // 济源
    (116.9565868378, 39.6513677208), // 天津市武清区河西务镇
    (100.4315185547, 21.7594997307), // 中国云南省西双版纳傣族自治州勐海县勐混镇
    (85.5670166016, 41.5548386631), // 中国新疆维吾尔自治区巴音郭楞蒙古自治州库尔勒市 普惠乡
    (117.9969406128, 27.7447712551), // 中国福建省南平市武夷山市 崇安街道
    (110.8520507813, 34.0526594214), // 河南省三门峡市卢氏县 瓦窑沟乡下河村
    (116.4811706543, 39.9255352817), // 北京市朝阳区 六里屯街道甜水园
    (116.3362348080, 40.0622912084), // 北京市昌平区 回龙观地区吉晟别墅社区
    (116.3362830877, 40.0594500522), // 北京市北京市昌平区 建材城西路65号
    (116.3325601816, 40.0397393499), // 北京市海淀区 清河街道 小米六期
    (117.0977783203, 36.5085323575), // 山东省济南市历城区
    (118.6358642578, 35.8356283889), // 山东省临沂市沂水县
    (119.7853088379, 36.3029520437), // 山东省潍坊市高密市柏城镇
    (119.8567199707, 36.2808142593), // 山东省青岛市胶州市胶西镇
    (120.3892135620, 36.2777698228), // 山东省青岛市城阳区流亭街道于家社区
    (116.3830447197, 39.9467430610), // 中国北京市北京市西城区鼓楼西大街171号
    (116.3830447197, 39.9467430610), // 中国北京市北京市西城区鼓楼西大街171号
    (116.3854265213, 39.9444070723) // 北京市西城区什刹海街道鼓西社区
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
    Admin.createDistrict( "山东省", "青岛市", "城阳区", 370200, 370200, 370214, Location(120.396256,36.307559))
  )

  "GeoSpatial.aroundAreas in scala" should "work" in {
    //    testSource.foreach {lnglat=>
    //      val area = AdminArea.aroundAreas(lnglat, CoordinateSystem.GCJ02, Seq(3000, 4000, 5000), chinaCellNode, chinaAreaData).areas
    //      if(area.nonEmpty)
    //        println(area)
    //    }


    //    val a = (116.3157212734,40.0540260711)
    val a = (116.3747191429, 39.9085182400)

    //    println(geoSpatial.determineAdmin(a, CoordinateSystem.GCJ02).toShort)
    //    println(geoSpatial.determineAdmin(a, true, CoordinateSystem.GCJ02).toShort)
    //    println(geoSpatial.determineArea(a, CoordinateSystem.GCJ02))
    //    println(geoSpatial.determineArea(a, CoordinateSystem.GCJ02, needStreet = true))
    //    println(geoSpatial.aroundMultiRadAreas(a, CoordinateSystem.GCJ02, Seq(1000, 2000, 3000, 4000, 5000)))
    //    println(geoSpatial.aroundAreas(a, CoordinateSystem.GCJ02))
  }

  "GeoSpatial.determineAdmin in oversea" should "success" in {
    //    val a = (90.417247,23.815664)
    //    val admin = geoSpatial.determineAdmin(a, CoordinateSystem.GCJ02)
    //    println(admin)
    //    assert(admin == Admin.createOversea)
  }

  "AAA in spark" should "work" in {
    //    val sparkConf = new SparkConf()
    //      .setAppName("test")
    //      .setMaster("local[2]")
    //      .setKryo()
    //
    //    val sparkContext = new SparkContext(sparkConf)
    //
    //    val gs = sparkContext.broadcast(geoSpatial)
    //
    //    sparkContext.parallelize(testSource).repartition(3).map(lnglat=>{
    //      gs.value.determineArea(lnglat, CoordinateSystem.GCJ02, needStreet = true).toShortNameString
    //    }).collect().foreach(println)

    //    val nameData:Array[AdminNameNode] = NameMappingDataProvider.loadSerializeFile()
    //
    //    println(nameData.length)
    //    nameData.flatMap(_.children).take(5).foreach(println)
    //
    //    val obj : Array[AdminNameData] = NameMappingDataProvider.loadTxtFile()
    //
    //    println("cost = " + (System.currentTimeMillis() - start))
    //
    //    println(obj.length)
    //    obj.take(5).foreach(println)

    //    val p = Point2d(118.875429,25.587189)

    // list为多边形边界,p为一个待测点

    //    val chinaData = DistrictLoader.loadChinaData.get

    import collection.JavaConversions._

//    val data = genCellAdmin()
//
//    data.take(10).foreach(println)
//    println(data.length)

    val chinaPoly = DistrictLoader.loadAMapJson("country/" + 100000 + ".json")
      .get.polyline.get
    val chinaCell = S2Utils.loadS2CellUnion(chinaPoly, min_level, min_level, 10000)
      .cellIds()
      .flatMap(s2CellId => {
        val cellLevel = getLevel(s2CellId.id)
        if (cellLevel == min_level) List(s2CellId.id())
        else childrenCellId(s2CellId, cellLevel, min_level).map(_.id()).toList
      })
      .distinct
      .map(s => (s, 1))
      .toMap

    val startS2: S2LatLng = S2LatLng.fromDegrees(0.8293, 72.004) //左下角
    val endS2: S2LatLng = S2LatLng.fromDegrees(55.8271, 137.8347) //右上角
    val rect: S2LatLngRect = new S2LatLngRect(startS2, endS2)
    val coverer: S2RegionCoverer = new S2RegionCoverer
    coverer.setMaxLevel(7)
    coverer.setMinLevel(7)
    val ss = coverer.getCovering(rect).cellIds()
      .flatMap(s2CellId => {
        val cellLevel = getLevel(s2CellId.id)
        if (cellLevel == min_level) List(s2CellId.id())
        else childrenCellId(s2CellId, cellLevel, min_level).map(_.id()).toList
      })
      .distinct
      .map(s => (s, chinaCell.getOrElse(s, -1)))
      .filter(_._2 == -1)

    ss.take(10).foreach(println)
    println(ss.size)
    //    val vertices = new util.ArrayList[S2Point]
    //    //注意，一般需要多边形内侧，此处需要按照逆时针顺序添加。
    //    vertices.add(S2LatLng.fromDegrees(lat, lng).toPoint)
    //    val s2Loop = new S2Loop(vertices)
    //    val polygon = new S2Polygon(s2Loop)


//    val boundaryIndex: Map[Long, List[Long]] =
//      AdminDataProvider.AdminLoader.loadBoundaryData
//      .keySet
//      .map(s => (new S2CellId(s).parent(min_level).id(), s))
//      .groupBy(_._1)
//      .map(s => (s._1, s._2.map(_._2).toList))
//
//    val cells = DistrictLoader.loadAMapJson("country/" + 100000 + ".json")
//      .get
//      .districts
//      .flatMap(province => {
//        val provinceData = DistrictLoader.loadAMapJson("province/" + province.adcode + ".json")
//        if (provinceData.nonEmpty) {
//          val cityDatas = provinceData.get
//            .districts
//            .flatMap(city => {
//              val cityData = DistrictLoader.loadAMapJson("city/" + city.adcode + ".json")
//              if (cityData.nonEmpty) {
//                val districtDatas = cityData.get.districts.map(dis => DistrictLoader.loadAMapJson("district/" + dis.adcode + ".json"))
//                if (districtDatas.exists(s => s.nonEmpty && s.get.polyline.nonEmpty)) districtDatas
//                else List(cityData)
//              } else {
//                val districtData = DistrictLoader.loadAMapJson("district/" + city.adcode + ".json")
//                if (districtData.nonEmpty && districtData.get.polyline.nonEmpty) List(districtData)
//                else List.empty
//              }
//            })
//          if (cityDatas.exists(s => s.nonEmpty && s.get.polyline.nonEmpty)) cityDatas
//          else List(provinceData)
//        } else List.empty
//      })
//      .flatMap(admin => {
//        if (admin.nonEmpty && admin.get.polyline.nonEmpty)
//          AdminLoader.loadS2CellUnion(admin.get.polyline.get, min_level, min_level, 1000)
//            .cellIds()
//            .flatMap(s2CellId => {
//              val cellLevel = getLevel(s2CellId.id)
//              if (cellLevel == min_level) List(s2CellId.id())
//              else childrenCellId(s2CellId, cellLevel, min_level).map(_.id()).toList
//            })
//            .distinct
//            .map(s => (s, boundaryIndex.getOrElse(s, List.empty)))
//            .filter(_._2.isEmpty)
//            .map(s => (s._1, admin.get.adcode))
//        else List.empty
//      })
//      .groupBy(_._1)
//      .filter(_._2.length == 1)
//      .flatMap(s => s._2)
//
//    println(cells.size)


    //    DistrictLoader.loadAMapJson("country/" + 100000 + ".json")
    //      .get
    //      .polyline
    //      .get
    //      .split('|').map(loopStr => {
    //      loopStr.split(';').map(coordStr => {
    //        val parts = coordStr.split(',')
    //        val lng = parts(0).toDouble
    //        val lat = parts(1).toDouble
    //        vertices.add(S2LatLng.fromDegrees(lat, lng))
    //      })
    //    })


    //    val gcj02LonLat = GeoUtils.toGCJ02(120.152983,36.119759, CoordinateSystem.WGS84)
    //    val gcj02LonLat2 = GeoUtils.toGCJ02(99.311893,22.005211, CoordinateSystem.WGS84)
    //
    //    println(gcj02LonLat)
    //
    //    val start = System.currentTimeMillis()

    //    val districtAdmin = geoSpatial.determineAdmin(gcj02LonLat, chinaData)
    //    val districtAdmin2 = geoSpatial.determineAdmin(gcj02LonLat2, chinaData)

    //    println("cost = " + (System.currentTimeMillis() - start))

    //    println(districtAdmin)
    //    println(districtAdmin2)
  }

  //  "BBB in spark" should "work" in {
  //    val sparkConf = new SparkConf()
  //      .setAppName("test")
  //      .setMaster("local[2]")
  //      .setKryo()
  //
  //    val sparkContext = new SparkContext(sparkConf)
  //
  //    val gs = sparkContext.broadcast(new GeoSpatial())
  //
  //    sparkContext.parallelize(testSource).repartition(3).map(lnglat=>{
  //      gs.value.determineArea(lnglat, CoordinateSystem.GCJ02, needStreet = true)
  //    }).collect().foreach(println)
  //  }
}
