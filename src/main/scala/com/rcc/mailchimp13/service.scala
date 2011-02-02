package com.rcc.mailchimp13

import org.json.{JSONArray, JSONObject}

import org.apache.commons.httpclient.{HttpClient, MultiThreadedHttpConnectionManager}
import org.apache.commons.httpclient.methods.{GetMethod, PostMethod, StringRequestEntity}

import java.util.{HashMap => JHashMap}

import scala.collection.JavaConversions._
import scala.collection.mutable.Buffer

trait Service {
  def lists(
      filter: Option[ListFilter] = None,
      start: Int = 0,
      limit: Int = 25): Seq[List]

  def listInterestGroupings(id: String): Seq[InterestGrouping]

  def listBatchSubscribe(
      id: String,
      batch: Seq[BatchSubscribe],
      doubleOptin: Boolean = true,
      updateExisting: Boolean = false,
      replaceInterests: Boolean = true): BatchSubscribeResponse

  def listUpdateMember(
      id: String,
      email: String,
      mergeVars: Seq[Tuple2[String, String]], // NVPs
      groupings: Seq[Tuple2[String, Seq[String]]], // ._1 is group name or ID
      emailType: EmailType.Value = EmailType.html,
      replaceInterests: Boolean = true): Boolean

  def listSubscribe(
      id: String,
      email: String,
      mergeVars: Seq[Tuple2[String, String]], // NVPs
      groupings: Seq[Tuple2[String, Seq[String]]], // ._1 is group name or ID
      emailType: EmailType.Value = EmailType.html,
      doubleOptin: Boolean = true,
      updateExisting: Boolean = false,
      replaceInterests: Boolean = true,
      sendWelcome: Boolean = false): Boolean

  def listUnsubscribe(
      id: String,
      email: String,
      delete: Boolean = false,
      sendGoodbye: Boolean = true,
      sendNotify: Boolean = true): Boolean
}

class ServiceImpl(val apikey: String, dataCenter: String) extends Service {
  var httpClient: HttpClient = new HttpClient(new MultiThreadedHttpConnectionManager)
  val endpoint = "https://" + dataCenter + ".api.mailchimp.com/1.3/"

  def lists(
      filter: Option[ListFilter] = None,
      start: Int = 0,
      limit: Int = 25): Seq[List] =
  {
    val params = new JSONObject

    params.put("start", start)
    params.put("limit", limit)

    filter match {
      case Some(f) => {
        val jf = new JSONObject

        f.id match {
          case Some(v) => jf.put("list_id", v)
          case None =>
        }

        f.name match {
          case Some(v) => jf.put("list_name", v)
          case None =>
        }

        f.defaultFromName match {
          case Some(v) => jf.put("from_name", v)
          case None =>
        }

        f.defaultFromEmail match {
          case Some(v) => jf.put("from_email", v)
          case None =>
        }

        f.defaultSubject match {
          case Some(v) => jf.put("from_subject", v)
          case None =>
        }

        f.createdBefore match {
          case Some(v) => jf.put("created_before", v)
          case None =>
        }

        f.createdAfter match {
          case Some(v) => jf.put("created_after", v)
          case None =>
        }

        jf.put("exact", f.exact)

        params.put("filters", jf)
      }
      case None =>
    }

    val json = new JSONObject(contact("lists", params))
    val listsJson = json.getJSONArray("data")
    val lists = Buffer[List]()
    for (i <- 0.until(listsJson.length)) {
      lists += List.fromJson(listsJson.getJSONObject(i))
    }
    return lists
  }

  def listInterestGroupings(id: String): Seq[InterestGrouping] = {
    val params = new JSONObject

    params.put("id", id)

    val jary = new JSONArray(contact("listInterestGroupings", params))
    val gs = Buffer[InterestGrouping]()
    for (i <- 0.until(jary.length)) {
      gs += InterestGrouping.fromJson(jary.getJSONObject(i))
    }
    return gs
  }

  def listBatchSubscribe(
      id: String,
      batch: Seq[BatchSubscribe],
      doubleOptin: Boolean = true,
      updateExisting: Boolean = false,
      replaceInterests: Boolean = true): BatchSubscribeResponse =
  {
    val params = new JSONObject

    params.put("id", id)
    params.put("double_optin", doubleOptin)
    params.put("update_existing", updateExisting)
    params.put("replace_interests", replaceInterests)

    val b = new JSONArray
    batch.foreach(s => {
      val json = new JSONObject
      json.put("EMAIL", s.email)
      json.put("EMAIL_TYPE", s.emailType.toString)

      s.mergeVars.foreach(t => { json.put(t._1, t._2) })

      if (s.groupings.nonEmpty) {
        val gs = new JSONArray

        s.groupings.foreach(g => {
          val t = new JSONObject
          t.put("name", g._1)
          t.put("groups", g._2.map(_.replaceAll(",", "\\\\,")).mkString(","))
          gs.put(t)
        })

        json.put("GROUPINGS", gs)
      }

      b.put(json)
    })
    params.put("batch", b)

    return BatchSubscribeResponse.fromJson(contact("listBatchSubscribe", params))
  }

  def listSubscribe(
      id: String,
      email: String,
      mergeVars: Seq[Tuple2[String, String]] = Nil,
      groupings: Seq[Tuple2[String, Seq[String]]] = Nil,
      emailType: EmailType.Value = EmailType.html,
      doubleOptin: Boolean = true,
      updateExisting: Boolean = false,
      replaceInterests: Boolean = true,
      sendWelcome: Boolean = false): Boolean =
  {
    val params = new JSONObject

    params.put("id", id)
    params.put("email_address", email)
    params.put("email_type", emailType.toString)
    params.put("double_optin", doubleOptin)
    params.put("update_existing", updateExisting)
    params.put("replace_interests", replaceInterests)
    params.put("send_welcome", sendWelcome)

    if (mergeVars.nonEmpty || groupings.nonEmpty) {
      val mvs = new JSONObject
      mergeVars.foreach(t => mvs.put(t._1, t._2))
      if (groupings.nonEmpty) {
        val gs = new JSONArray
        groupings.foreach(g => {
          val t = new JSONObject
          t.put("name", g._1)
          t.put("groups", g._2.map(_.replaceAll(",", "\\\\,")).mkString(","))
          gs.put(t)
        })
        mvs.put("GROUPINGS", gs)
      }
      params.put("merge_vars", mvs)
    }

    val body = contact("listSubscribe", params)

    return body == "true"
  }

  def listUpdateMember(
      id: String,
      email: String,
      mergeVars: Seq[Tuple2[String, String]], // NVPs
      groupings: Seq[Tuple2[String, Seq[String]]], // ._1 is group name or ID
      emailType: EmailType.Value = EmailType.html,
      replaceInterests: Boolean = true): Boolean =
  {
    val params = new JSONObject

    params.put("id", id)
    params.put("email_address", email)
    params.put("email_type", emailType.toString)
    params.put("replace_interests", replaceInterests)

    if (mergeVars.nonEmpty || groupings.nonEmpty) {
      val mvs = new JSONObject
      mergeVars.foreach(t => mvs.put(t._1, t._2))
      if (groupings.nonEmpty) {
        val gs = new JSONArray
        groupings.foreach(g => {
          val t = new JSONObject
          t.put("name", g._1)
          t.put("groups", g._2.map(_.replaceAll(",", "\\\\,")).mkString(","))
          gs.put(t)
        })
        mvs.put("GROUPINGS", gs)
      }
      params.put("merge_vars", mvs)
    }

    val body = contact("listUpdateMember", params)

    return body == "true"
  }

  def listUnsubscribe(
      id: String,
      email: String,
      delete: Boolean,
      sendGoodbye: Boolean,
      sendNotify: Boolean): Boolean =
  {
    val params = new JSONObject

    params.put("id", id)
    params.put("email_address", email)
    params.put("delete_member", delete)
    params.put("send_goodbye", sendGoodbye)
    params.put("send_notify", sendNotify)

    val body = contact("listUnsubscribe", params)

    return body == "true"
  }

  def contact(method: String, params: JSONObject): String = {
    val post = new PostMethod(endpoint + "?method=" + method)

    params.put("apikey", apikey)
    params.put("output", "json")

    post.setRequestEntity(new StringRequestEntity(params.toString, "application/json", "utf-8"))

    httpClient.executeMethod(post) match {
      case 200 => post.getResponseBodyAsString
      case ret: Int => { error("Unexpected HTTP status code: " + ret) }
    }
  }
}
