package com.dengxq.lnglat2Geo.utils

import java.io._

object ObjectSerializer {
  def serialize[T](o: T, fname: String) {
    val bos = new FileOutputStream(fname)//基于磁盘文件流的序列化
    val oos = new ObjectOutputStream(bos)
    oos.writeObject(o)
    oos.close()
  }

  def deserialize[T](fname: String): T = {
    val bis = new FileInputStream(fname)
    val ois = new ObjectInputStream(bis)
    ois.readObject.asInstanceOf[T]
  }

  def deserialize[T](bis: InputStream): T = {
    val ois = new ObjectInputStream(bis)
    ois.readObject.asInstanceOf[T]
  }

  def serialize[T](o: T): Array[Byte] = {
    val bos = new ByteArrayOutputStream()
    val oos = new ObjectOutputStream(bos)
    oos.writeObject(o)
    oos.close()
    bos.toByteArray
  }

  def deserialize[T](bytes: Array[Byte]): T={
    val bis=new ByteArrayInputStream(bytes)
    val ois=new ObjectInputStream(bis)
    ois.readObject.asInstanceOf[T]
  }
}
