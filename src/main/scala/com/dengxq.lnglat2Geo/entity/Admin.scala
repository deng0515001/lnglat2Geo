package com.dengxq.lnglat2Geo.entity

import com.dengxq.lnglat2Geo.utils.AdminUtils

case class Admin(country: String,
                 province: String,
                 city: String,
                 district: String,
                 town: String,
                 level: String,
                 countryCode: String,
                 provinceCode: Int,
                 cityCode: Int,
                 districtCode: Int,
                 townCode: Int,
                 center: Location = null
                ) {

  def hasCenter: Boolean = center != Admin.UNKNOWN_LOCATION_VAL
  def hasProvince: Boolean = province != Admin.UNKNOWN_NAME_VAL
  def hasCity: Boolean = city != Admin.UNKNOWN_NAME_VAL
  def hasDistrict: Boolean = district != Admin.UNKNOWN_NAME_VAL
  def hasCityId: Boolean = cityCode != Admin.UNKNOWN_ID_VAL
  def hasDistrictId: Boolean = districtCode != Admin.UNKNOWN_ID_VAL
  def hasTown: Boolean = town != Admin.UNKNOWN_NAME_VAL
  def shortProvince :String = AdminUtils.shortProvince(province)
  def shortCity :String = AdminUtils.shortCity(city)
  def toShort: Admin = Admin(country,
    AdminUtils.shortProvince(province),
    AdminUtils.shortCity(city),
    AdminUtils.shortDistrict(district),
    AdminUtils.shortStreet(town),
    level, countryCode, provinceCode, cityCode, districtCode, townCode, center)
  def toNameString: String = s"$country${if(hasProvince) province else ""}${if(hasCity) city else ""}${if(hasDistrict) district else ""}${if(hasTown) town else ""}"
}

private[lnglat2Geo] object Admin {
  private final val CHINA_NAME = "中国"
  private final val CHINA_ID = "CN"
  private final val OVERSEA_NAME_VAL = "海外"
  private final val UNKNOWN_NAME_VAL = "未知"
  private final val UNKNOWN_ID_VAL = -1
  private final val UNKNOWN_LOCATION_VAL = null

  def createOversea = Admin(OVERSEA_NAME_VAL,
    province = OVERSEA_NAME_VAL,
    city = OVERSEA_NAME_VAL,
    district = OVERSEA_NAME_VAL,
    town = OVERSEA_NAME_VAL,
    level = AdminLevel.Oversea,
    countryCode = "",
    provinceCode = UNKNOWN_ID_VAL,
    cityCode = UNKNOWN_ID_VAL,
    districtCode = UNKNOWN_ID_VAL,
    townCode = UNKNOWN_ID_VAL,
    center = UNKNOWN_LOCATION_VAL
  )

  def createCountry(country:String, countryID:String, center: Location) = Admin(
    country,
    province = UNKNOWN_NAME_VAL,
    city = UNKNOWN_NAME_VAL,
    district = UNKNOWN_NAME_VAL,
    town = UNKNOWN_NAME_VAL,
    level = AdminLevel.Country,
    countryCode = countryID,
    provinceCode = UNKNOWN_ID_VAL,
    cityCode = UNKNOWN_ID_VAL,
    districtCode = UNKNOWN_ID_VAL,
    townCode = UNKNOWN_ID_VAL,
    center = center
  )

  def createProvince(province: String, provinceId: Int, center: Location) = Admin(
    country = CHINA_NAME,
    province = province,
    city = UNKNOWN_NAME_VAL,
    district = UNKNOWN_NAME_VAL,
    town = UNKNOWN_NAME_VAL,
    level = AdminLevel.Province,
    countryCode = CHINA_ID,
    provinceCode = provinceId,
    cityCode = UNKNOWN_ID_VAL,
    districtCode = UNKNOWN_ID_VAL,
    townCode = UNKNOWN_ID_VAL,
    center = center
  )

   def createCity(province: String, city: String, provinceId: Int, cityId: Int, center: Location) = Admin(
    country = CHINA_NAME,
    province = province,
    city = city,
    district = UNKNOWN_NAME_VAL,
    town = UNKNOWN_NAME_VAL,
    level = AdminLevel.City,
    countryCode = CHINA_ID,
    provinceCode = provinceId,
    cityCode = cityId,
    districtCode = UNKNOWN_ID_VAL,
    townCode = UNKNOWN_ID_VAL,
    center = center
  )

  def createProvincialCity(province: String, city: String, provinceId: Int, cityId: Int, center: Location) = Admin(
    country = CHINA_NAME,
    province = province,
    city = city,
    district = city,
    town = UNKNOWN_NAME_VAL,
    level = AdminLevel.ProvincialCity,
    countryCode = CHINA_ID,
    provinceCode = provinceId,
    cityCode = cityId,
    districtCode = cityId,
    townCode = UNKNOWN_ID_VAL,
    center = center
  )

  def createDistrict(province: String, city: String, district: String,
                                         provinceId:Int, cityId: Int, districtId: Int, center: Location) = Admin(
    country = CHINA_NAME,
    province = province,
    city = city,
    district = district,
    town = UNKNOWN_NAME_VAL,
    level = AdminLevel.District,
    countryCode = CHINA_ID,
    provinceCode = provinceId,
    cityCode = cityId,
    districtCode = districtId,
    townCode = UNKNOWN_ID_VAL,
    center = center
  )

  def createStreet(province: String, city: String, district: String, town: String,
                                       provinceId:Int, cityId: Int, districtId: Int, streetId: Int, center: Location) = Admin(
    country = CHINA_NAME,
    province = province,
    city = city,
    district = district,
    town = town,
    level = AdminLevel.Street,
    countryCode = CHINA_ID,
    provinceCode = provinceId,
    cityCode = cityId,
    districtCode = districtId,
    townCode = streetId,
    center = center
  )

}
