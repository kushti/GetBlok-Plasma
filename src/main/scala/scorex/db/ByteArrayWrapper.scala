package scorex.db

import scorex.ByteUtils

import java.io.Serializable
import java.util

/**
  * Wraps byte array and provides hashCode, equals and compare methods.
  */
case class ByteArrayWrapper(data: Array[Byte])
  extends Serializable
    with Comparable[ByteArrayWrapper]
    with Ordered[ByteArrayWrapper] {

  /** alternative constructor which takes array size and creates new empty array */
  def this(size:Int) = this(new Array[Byte](size))

  require(data != null)

  //TODO wrapped data immutable?

  override def equals(o: Any): Boolean =
    o.isInstanceOf[ByteArrayWrapper] &&
      util.Arrays.equals(data, o.asInstanceOf[ByteArrayWrapper].data)

  override def hashCode: Int = ByteUtils.byteArrayHashCode(data)

  override def compareTo(o: ByteArrayWrapper): Int = ByteUtils.BYTE_ARRAY_COMPARATOR.compare(this.data, o.data)

  override def compare(that: ByteArrayWrapper): Int = compareTo(that)

  override def toString: String = {
    val v = if (data.length == 8) {
      //if size is 8, display as a number
      ByteUtils.getLong(data, 0).toString + "L"
    } else {
      javax.xml.bind.DatatypeConverter.printHexBinary(data)
    }
    getClass.getSimpleName + "[" + v + "]"
  }
}
