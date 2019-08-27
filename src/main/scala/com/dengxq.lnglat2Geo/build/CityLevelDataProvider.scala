package com.dengxq.lnglat2Geo.build

import scala.io.{Codec, Source}

object CityLevelDataProvider {

  def csv2Map: Map[String, String] = {
    val fname = "/china/citylevels"
    val url = getClass.getResource(fname)
    Source.fromURL(url)(Codec.UTF8)
      .getLines()
      .flatMap(line => {
        val Array(name, adCode, cityLevel) = line.split(",")
        List((name, cityLevel), (adCode, cityLevel))
      })
      .toMap
  }
}
