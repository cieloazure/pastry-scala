package pastry

import java.security.MessageDigest
import util.control.Breaks._

class PastryNodeId(val uniqueTrait: String, val length: Int = 16, val base: Int = 16, val algorithm: String = "sha1")
  extends Ordered[PastryNodeId] {
  val hashCalc = MessageDigest.getInstance(algorithm);
  val truncateLen: Int = if (length > hashCalc.getDigestLength() || length < 0) {
    0
  } else {
    hashCalc.getDigestLength() - length;
  }

  private val _byteId: Array[Byte] = hashCalc.digest(uniqueTrait.getBytes()).dropRight(truncateLen)

  override def compare(that: PastryNodeId): Int = {
    this.getIntBase10.compareTo(that.getIntBase10)
  }

  def getHex: String = {
    _byteId.map("%02x".format(_)).mkString
  }

  def byteId=  _byteId

  def findCommonPrefix(other: PastryNodeId): Int = {
    var prefix = 0
    breakable {
      for(pair <- getHex.zip(other.getHex)){
        if(pair._1 == pair._2) {
          prefix += 1
        } else {
          break
        }
      }
    }
    prefix
  }

  def getIntBase10: BigInt = {
    getHex.toList.map("0123456789abcdef".indexOf(_)).reduceLeft(_ * 16 + _).abs
  }
}
