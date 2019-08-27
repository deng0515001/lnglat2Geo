package com.dengxq.lnglat2Geo

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class AdminDataProviderTest extends FlatSpec {

  "Node data" should "Serialize & Deserialize" in {
//    Upgrade.upgradeAdminBoundary()
//    Upgrade.upgradeAdminBoundaryCell()
//    Upgrade.upgradeTownship()
//    Upgrade.upgradeChinaAdmin()
//    Upgrade.upgradeBusinessAreaData()

    GeoTrans.normalizeName("","攀枝花","","")
      .foreach(println)
//adminData.values.filter(s => s.level.equals(DistrictLevel.Province)).filter(s => s.name.equals("香港") || s.shortName.equals("香港"))
//    .flatMap(s => s.children.map(adCode => adminData.get(adCode).orNull)).foreach(println)
    GeoTrans.normalizeName("","北京","海淀","")
      .foreach(println)
//    val result = GeoSpatial.aroundBusinessAreas(116.36450116862099,39.9260007939375, 4000, CoordinateSystem.WGS84)
//    println(result)
  }

}
