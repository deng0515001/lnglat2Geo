package com.dengxq.lnglat2Geo.entity

import com.dengxq.lnglat2Geo.entity.AdminLevel.AdminLevel
import com.dengxq.lnglat2Geo.utils.AdminUtils

case class Admin(country: String,
                 province: String,
                 city: String,
                 district: String,
                 street: String,
                 level: AdminLevel,
                 countryID: String,
                 provinceID: Int,
                 cityID: Int,
                 districtID: Int,
                 streetID: Int,
                 center: Location = null
                ) {

  def hasCenter: Boolean = center != Admin.UNKNOWN_LOCATION_VAL
  def hasProvince: Boolean = province != Admin.UNKNOWN_NAME_VAL
  def hasCity: Boolean = city != Admin.UNKNOWN_NAME_VAL
  def hasDistrict: Boolean = district != Admin.UNKNOWN_NAME_VAL
  def hasCityId: Boolean = cityID != Admin.UNKNOWN_ID_VAL
  def hasDistrictId: Boolean = districtID != Admin.UNKNOWN_ID_VAL
  def hasStreet: Boolean = street != Admin.UNKNOWN_NAME_VAL
  def shortProvince :String = AdminUtils.shortProvince(province)
  def shortCity :String = AdminUtils.shortCity(city)
  def toShort: Admin = Admin(country,
    AdminUtils.shortProvince(province),
    AdminUtils.shortCity(city),
    AdminUtils.shortDistrict(district),
    AdminUtils.shortStreet(street),
    level, countryID, provinceID, cityID, districtID, streetID, center)
  def toNameString: String = s"$country${if(hasProvince) province else ""}${if(hasCity) city else ""}${if(hasDistrict) district else ""}${if(hasStreet) street else ""}"
}

object Admin {
  private final val CHINA_NAME = "中国"
  private final val CHINA_ID = "CN"
  private final val OVERSEA_NAME_VAL = "海外"
  private final val UNKNOWN_NAME_VAL = "未知"
  private final val UNKNOWN_ID_VAL = -1
  private final val UNKNOWN_LOCATION_VAL = null

  private[lnglat2Geo] def createOversea = Admin(OVERSEA_NAME_VAL,
    province = OVERSEA_NAME_VAL,
    city = OVERSEA_NAME_VAL,
    district = OVERSEA_NAME_VAL,
    street = OVERSEA_NAME_VAL,
    level = AdminLevel.Oversea,
    countryID = "",
    provinceID = UNKNOWN_ID_VAL,
    cityID = UNKNOWN_ID_VAL,
    districtID = UNKNOWN_ID_VAL,
    streetID = UNKNOWN_ID_VAL,
    center = UNKNOWN_LOCATION_VAL
  )

  private[lnglat2Geo] def createCountry(country:String, countryID:String, center: Location) = Admin(
    country,
    province = UNKNOWN_NAME_VAL,
    city = UNKNOWN_NAME_VAL,
    district = UNKNOWN_NAME_VAL,
    street = UNKNOWN_NAME_VAL,
    level = AdminLevel.Country,
    countryID = countryID,
    provinceID = UNKNOWN_ID_VAL,
    cityID = UNKNOWN_ID_VAL,
    districtID = UNKNOWN_ID_VAL,
    streetID = UNKNOWN_ID_VAL,
    center = center
  )

  private[lnglat2Geo] def createProvince(province: String, provinceId: Int, center: Location) = Admin(
    country = CHINA_NAME,
    province = province,
    city = UNKNOWN_NAME_VAL,
    district = UNKNOWN_NAME_VAL,
    street = UNKNOWN_NAME_VAL,
    level = AdminLevel.Province,
    countryID = CHINA_ID,
    provinceID = provinceId,
    cityID = UNKNOWN_ID_VAL,
    districtID = UNKNOWN_ID_VAL,
    streetID = UNKNOWN_ID_VAL,
    center = center
  )

  private[lnglat2Geo] def createCity(province: String, city: String, provinceId: Int, cityId: Int, center: Location) = Admin(
    country = CHINA_NAME,
    province = province,
    city = city,
    district = UNKNOWN_NAME_VAL,
    street = UNKNOWN_NAME_VAL,
    level = AdminLevel.City,
    countryID = CHINA_ID,
    provinceID = provinceId,
    cityID = cityId,
    districtID = UNKNOWN_ID_VAL,
    streetID = UNKNOWN_ID_VAL,
    center = center
  )

  private[lnglat2Geo] def createProvincialCity(province: String, city: String, provinceId: Int, cityId: Int, center: Location) = Admin(
    country = CHINA_NAME,
    province = province,
    city = city,
    district = city,
    street = UNKNOWN_NAME_VAL,
    level = AdminLevel.ProvincialCity,
    countryID = CHINA_ID,
    provinceID = provinceId,
    cityID = cityId,
    districtID = cityId,
    streetID = UNKNOWN_ID_VAL,
    center = center
  )

  private[lnglat2Geo] def createDistrict(province: String, city: String, district: String,
                                         provinceId:Int, cityId: Int, districtId: Int, center: Location) = Admin(
    country = CHINA_NAME,
    province = province,
    city = city,
    district = district,
    street = UNKNOWN_NAME_VAL,
    level = AdminLevel.District,
    countryID = CHINA_ID,
    provinceID = provinceId,
    cityID = cityId,
    districtID = districtId,
    streetID = UNKNOWN_ID_VAL,
    center = center
  )

  private[lnglat2Geo] def createStreet(province: String, city: String, district: String, street: String,
                                       provinceId:Int, cityId: Int, districtId: Int, streetId: Int, center: Location) = Admin(
    country = CHINA_NAME,
    province = province,
    city = city,
    district = district,
    street = street,
    level = AdminLevel.Street,
    countryID = CHINA_ID,
    provinceID = provinceId,
    cityID = cityId,
    districtID = districtId,
    streetID = streetId,
    center = center
  )

}
