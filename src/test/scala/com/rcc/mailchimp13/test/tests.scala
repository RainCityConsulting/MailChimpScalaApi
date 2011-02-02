package com.rcc.mailchimp13.test

import com.rcc.mailchimp13._

import org.junit.Assert._
import org.junit.Test

class BeanTests {
  @Test
  def testLists {
    val list = List.fromJson("""{"id":"c8bc7dc072","web_id":577325,"name":"Pearl Jam","date_created":"2010-07-15 19:00:36","email_type_option":false,"use_awesomebar":false,"default_from_name":"Pearl Jam","default_from_email":"noreply@lists.pearljam.com","default_subject":"Pearl Jam Amongst the Waves Download","default_language":"en","list_rating":3,"stats":{"member_count":3936,"unsubscribe_count":158,"cleaned_count":0,"member_count_since_send":15710,"unsubscribe_count_since_send":171,"cleaned_count_since_send":0,"campaign_count":0,"grouping_count":0,"group_count":0,"merge_var_count":0,"avg_sub_rate":2380,"avg_unsub_rate":25,"target_sub_rate":25,"open_rate":0,"click_rate":0},"modules":[]}""")
    assertEquals("c8bc7dc072", list.id)
    assertEquals(15710, list.stats.memberCountSinceSend)
  }
}

class ServiceTests {
  val service = new ServiceImpl("0ec601a9a8b165e5e3a416aedb2e7fae-us1", "us1")

  @Test
  def testLists {
    val lists = service.lists()
    val lists2 = service.lists(None, 0, 1)
    val f = new ListFilter(None, Some("List Name"), None, None, None, None, None, true)
    val lists3 = service.lists(Some(f), 0, 1)
  }

  @Test
  def testListInterestGroupings {
    val groupings = service.listInterestGroupings("14b51aca96")
  }

  @Test
  def testBatchSubscribe {
    val batch = Seq(
        new BatchSubscribe(
            "ian@1321.org", EmailType.html, Seq(("FNAME", "Ian33")), Nil))

    service.listBatchSubscribe(
        "14b51aca96",
        batch,
        false,
        true,
        true)
  }

  @Test
  def testSubscribe {
    val ret = service.listSubscribe(
        "14b51aca96",
        "ian@1321.org",
        Seq(("LNAME", "Shafer2")),
        Seq(("Interested in ...", Seq("Products"))),
        EmailType.mobile,
        false, true, true, false)
    assertTrue(ret)
  }
}
