package com.github.swagger.scala3enum.converter.adt

import io.swagger.v3.core.converter.ModelConverters
import io.swagger.v3.oas.models.media.{ArraySchema, Schema, StringSchema}
import org.scalatest.OptionValues
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.jdk.CollectionConverters.*
import scala.reflect.Enum

class SwaggerAdtConverterSpec extends AnyWordSpec with Matchers with OptionValues {
  "SwaggerScala3EnumModelConverter" should {
    "get model for Car" in {
      val converter = ModelConverters.getInstance()
      val schemas = converter.readAll(classOf[Car]).asScala.toMap
      val model = findModel(schemas, "Car")
      model should be(defined)
      model.get.getProperties should not be (null)
      val field = model.value.getProperties.get("color")
      field shouldBe a[StringSchema]
      nullSafeList(field.asInstanceOf[StringSchema].getEnum) shouldEqual Seq("Red", "Green", "Blue")
      nullSafeList(field.getRequired) shouldBe empty
      nullSafeList(model.value.getRequired) shouldEqual Seq("color", "make")
    }
    "get model for ColorSet" in {
      val converter = ModelConverters.getInstance()
      val schemas = converter.readAll(classOf[ColorSet]).asScala.toMap
      val model = findModel(schemas, "ColorSet")
      model should be (defined)
      model.get.getProperties should not be (null)
      val field = model.value.getProperties.get("set")
      field shouldBe an [ArraySchema]
      nullSafeList(field.asInstanceOf[ArraySchema].getEnum) shouldEqual Seq("Red", "Green", "Blue")
    }
  }

  private def findModel(schemas: Map[String, Schema[_]], name: String): Option[Schema[_]] = {
    schemas.get(name) match {
      case Some(m) => Some(m)
      case None =>
        schemas.keys.find { case k => k.startsWith(name) } match {
          case Some(key) => schemas.get(key)
          case None => schemas.values.headOption
        }
    }
  }

  private def nullSafeList[T](list: java.util.List[T]): List[T] = Option(list) match {
    case None => List[T]()
    case Some(l) => l.asScala.toList
  }
}
