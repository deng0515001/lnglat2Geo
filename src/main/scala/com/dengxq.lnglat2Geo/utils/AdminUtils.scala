package com.dengxq.lnglat2Geo.utils

import com.dengxq.lnglat2Geo.entity.DistrictLevel
import com.dengxq.lnglat2Geo.entity.DistrictLevel.DistrictLevel

object AdminUtils {
  private final val NATIONS = "阿昌族,鄂温克族,傈僳族,水族,白族,高山族,珞巴族,塔吉克族,保安族,仡佬族,满族,塔塔尔族,布朗族,哈尼族,毛南族,土家族,布依族,哈萨克族,门巴族,土族,朝鲜族,汉族,蒙古族,佤族,达斡尔族,赫哲族,苗族,维吾尔族,傣族,回族,仫佬族,乌孜别克族,德昂族,基诺族,纳西族,锡伯族,东乡族,京族,怒族,瑶族,侗族,景颇族,普米族,彝族,独龙族,柯尔克孜族,羌族,裕固族,俄罗斯族,拉祜族,撒拉族,藏族,鄂伦春族,黎族,畲族,壮族".split(",")

  private final val p1 = """(.+)(?:省|市)$""".r
  private final val p2 = """(.+)自治区""".r
  private final val p3 = """(.+)特别行政区""".r

  private final val c0 = """^(.{2})$""".r // 2 长度为2的 "东区" "南区"
  private final val c1 = """(.+)自治州$""".r // 30 自治州
  private final val c2 = """(.+)[市|盟|州]$""".r // 304 地级市, 盟; + 1恩施州
  private final val c3 = """(.+)地区$""".r // 8 地区
  private final val c4 = """(.+)(?:群岛|填海区)$""".r // 2 东沙群岛
  private final val c5 = """(.+[^地郊城堂])区$""".r // 20 港澳 不含"东区" "南区"2个字的
  private final val c6 = """(.+)(?:城区|郊县)$""".r // 6 九龙城区,上海城区,天津城区,北京城区,重庆城区,重庆郊县
  private final val c7 = """(.+[^郊])县$""".r // 12 台湾的xx县

  private final val d0 = """^(.{2})$""".r // 2 长度为2的 "随县"
  private final val d1 = """(.+)[市]$""".r // 304 城区 “赤水市”
  private final val d2 = """(.+)自治县$""".r // 30 自治县
  private final val d3 = """(.+)自治州直辖$""".r // 30 自治州直辖 "海西蒙古族藏族自治州直辖"
  private final val d4 = """(.+)[区|县]$""".r // 8 区县
  private final val d5 = """(.+)(?:乡|镇|街道)$""".r // 8 乡镇|街道

  private final val s0 = """^(.{2})$""".r
  private final val s1 = """(.+)(?:特别行政管理区|街道办事处|旅游经济特区|民族乡|地区街道)$""".r
  private final val s2 = """(.+)(?:镇|乡|村|街道|苏木|老街|管理区|区公所|苏木|办事处|社区|经济特区|行政管理区)$""".r

  def shortAdmin(name:String, level:DistrictLevel): String = {
    level match {
      case DistrictLevel.Province => shortProvince(name)
      case DistrictLevel.City => shortCity(name)
      case DistrictLevel.District => shortDistrict(name)
      case DistrictLevel.Street => shortStreet(name)
      case _ => name
    }
  }

  def shortProvince(province: String) :String = {
    province match {
      case p1(x) => x
      case p2(x) => if(x== "内蒙古") x else replaceNations(x)
      case p3(x) => x
      case _ => province
    }
  }

  private def replaceNations(ncity:String) = (Array(ncity) ++ NATIONS).reduce((x, y) => x.replaceAll(y, "").replaceAll(if(y.length > 2) y.replaceAll("族", "") else "", ""))

  private def replaceNationsNotEmpty(name: String) =
    (Array(name) ++ NATIONS).reduce((x, y) => {
      val x2 = x.replaceAll(y, "").replaceAll(if (y.length > 2) y.replaceAll("族", "") else "", "")
      if(x2.isEmpty) x else x2
    })
  private[lnglat2Geo] def shortCityImp(city: String) :(String, Int) = {
    // 总数 383
    city match {
      case c0(x) => (x, 0)
      case c1(x) => (replaceNations(x), 2)
      case c2(x) => if(x == "襄樊") ("襄阳",1) else (x, 1)
      case c3(x) => (x,3)
      case c4(x) => (x,4)
      case c5(x) => (x,5)
      case c6(x) => (x,6)
      case c7(x) => (x,7)
      case _ => (city, -1)
    }
  }

  private[lnglat2Geo] def shortDistrictImp(district: String) :(String, Int) = {
    // 总数 2963 56个内蒙八旗和新疆兵团没有处理
    district match {
      case d0(x) => (x, 0)
      case d1(x) => (x, 1)
      case d2(x) => (replaceNations(x), 2)
      case d3(x) => (replaceNations(x), 3)
      case d4(x) => (x,4)
      case d5(x) => (x,5)
      case _ => (district, -1)
    }
  }

  private[lnglat2Geo] def shortStreetImp(street: String) :(String, Int) = {
    // 总数 42387
    // 柘城县邵园乡人民政府, 保安镇, 鹅湖镇人民政府, 东风地区
    street match {
      case s0(x) => (x, 0)
      case s1(x) => (replaceNationsNotEmpty(x), 1)
      case s2(x) => (replaceNationsNotEmpty(x), 2)
      case _ => (street, -1)
    }
  }

  def shortCity(city: String):String = shortCityImp(city)._1

  def shortDistrict(district: String):String = shortDistrictImp(district)._1

  def shortStreet(street: String):String = shortStreetImp(street)._1
}
