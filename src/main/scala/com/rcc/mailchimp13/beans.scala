package com.rcc.mailchimp13

import org.json.{JSONArray, JSONObject}

import java.text.SimpleDateFormat
import java.util.Date

import scala.collection.mutable.Buffer

class BatchSubscribeError(val code: String, val message: String, val email: String)

object BatchSubscribeError {
  def fromJson(jsonString: String): BatchSubscribeError = fromJson(new JSONObject(jsonString))

  def fromJson(json: JSONObject): BatchSubscribeError = {
    new BatchSubscribeError(
        json.getString("code"),
        json.getString("message"),
        json.getString("email"))
  }
}

class BatchSubscribeResponse(
    val addCount: Int,
    val updateCount: Int,
    val errorCount: Int,
    val errors: Seq[BatchSubscribeError])

object BatchSubscribeResponse {
  def fromJson(jsonString: String): BatchSubscribeResponse = fromJson(new JSONObject(jsonString))

  def fromJson(json: JSONObject): BatchSubscribeResponse = {
    val errors = Buffer[BatchSubscribeError]()
    val jary = json.getJSONArray("errors")
    for (i <- 0.until(jary.length)) {
      errors += BatchSubscribeError.fromJson(jary.getJSONObject(i))
    }
    new BatchSubscribeResponse(
        json.getInt("add_count"), json.getInt("update_count"), json.getInt("error_count"), errors)
  }
}

class BatchSubscribe(
    val email: String,
    val emailType: EmailType.Value,
    val mergeVars: Seq[Tuple2[String, String]], // NVPs
    val groupings: Seq[Tuple2[String, Seq[String]]]) // ._1 is group name or ID

class InterestGroup(val bit: Int, val name: String, val subscriberCount: Int)

object InterestGroup {
  def fromJson(jsonString: String): InterestGroup = fromJson(new JSONObject(jsonString))

  def fromJson(json: JSONObject): InterestGroup = {
    new InterestGroup(json.getInt("bit"), json.getString("name"), json.getInt("subscribers"))
  }
}

class InterestGrouping(val id: Int, val name: String, val groups: Seq[InterestGroup])

object InterestGrouping {
  def fromJson(jsonString: String): InterestGrouping = fromJson(new JSONObject(jsonString))

  def fromJson(json: JSONObject): InterestGrouping = {
    val groups = Buffer[InterestGroup]()
    val jary = json.getJSONArray("groups")
    for (i <- 0.until(jary.length)) {
      groups += InterestGroup.fromJson(jary.getJSONObject(i))
    }
    new InterestGrouping(json.getInt("id"), json.getString("name"), groups)
  }
}

object EmailType extends Enumeration {
  type EmailType = Value

  val html = Value("html")
  val text = Value("text")
  val mobile = Value("mobile")
}

object ListStats {
  def fromJson(jsonStr: String): ListStats = fromJson(new JSONObject(jsonStr))

  def fromJson(json: JSONObject): ListStats = {
    new ListStats(
        json.getInt("member_count"),
        json.getInt("unsubscribe_count"),
        json.getInt("cleaned_count"),
        json.getInt("member_count_since_send"),
        json.getInt("unsubscribe_count_since_send"),
        json.getInt("cleaned_count_since_send"),
        json.getInt("campaign_count"),
        json.getInt("grouping_count"),
        json.getInt("group_count"),
        json.getInt("merge_var_count"),
        option(json.optInt("avg_sub_rate", -1)),
        option(json.optInt("avg_unsub_rate", -1)),
        option(json.optInt("target_sub_rate", -1)),
        option(json.optInt("open_rate", -1)),
        option(json.optInt("click_rate", -1))
    )
  }

  private def option(v: Int): Option[Int] = if (v == -1) {
    None
  } else {
    Some(v)
  }
}

class ListStats(
   val memberCount: Int,
   val unsubCount: Int,
   val cleanedCount: Int,
   val memberCountSinceSend: Int,
   val unsubCountSinceSend: Int,
   val cleanedCountSinceSend: Int,
   val campaignCount: Int,
   val groupingCount: Int,
   val groupCount: Int,
   val mergeVarCount: Int,
   val avgSubRate: Option[Int],
   val avgUnsubRate: Option[Int],
   val targetSubRate: Option[Int],
   val openRate: Option[Int],
   val clickRate: Option[Int])

object List {
  val dateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss")

  def fromJson(jsonStr: String): List = {
    val json = new JSONObject(jsonStr)
    fromJson(json)
  }

  def fromJson(json: JSONObject): List = {
    new List(
        json.getString("id"),
        json.getInt("web_id"),
        json.getString("name"),
        toDate(json.getString("date_created")),
        json.getBoolean("email_type_option"),
        json.getBoolean("use_awesomebar"),
        json.getString("default_from_name"),
        json.getString("default_from_email"),
        json.getString("default_subject"),
        json.getString("default_language"),
        json.getInt("list_rating"),
        ListStats.fromJson(json.getJSONObject("stats")),
        moduleList(json.getJSONArray("modules"))
    )
  }

  private def toDate(dateStr: String): Date = dateFormat.parse(dateStr)

  private def moduleList(jary: JSONArray): Seq[String] = Seq()
}

class List(
    val id: String,
    val webId: Int,
    val name: String,
    val dateCreated: Date,
    val hasEmailTypeOption: Boolean,
    val useAwesomebar: Boolean,
    val defaultFromName: String,
    val defaultFromEmail: String,
    val defaultSubject: String,
    val defaultLanguage: String,
    val rating: Int,
    val stats: ListStats,
    val modules: Seq[String])

class ListFilter(
    val id: Option[String],
    val name: Option[String],
    val defaultFromName: Option[String],
    val defaultFromEmail: Option[String],
    val defaultSubject: Option[String],
    val createdBefore: Option[Date],
    val createdAfter: Option[Date],
    val exact: Boolean)
