package org.sunbird.viewer.competency

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

/** Skills from a taxonomy framework (USF): a flat `skill` category, tagged on content by NAME. */
class TaxonomySkillsSpec extends AnyFlatSpec with Matchers {

  import CompetencyFrameworkUtil._

  // Shape of the live USF read: flat categories, skills under `skill`, no competency tree.
  private val usf =
    """{"result":{"framework":{"identifier":"USF","categories":[
         {"code":"industry","terms":[{"code":"it","name":"Information Technology"}]},
         {"code":"domain","terms":[{"code":"sd","name":"Software Development"}]},
         {"code":"skill","terms":[
            {"code":"dv","name":"Data Visualization"},
            {"code":"pp","name":"Python Programming"}]}]}}}"""

  // A competency framework that ALSO carries a `skill` category as an association tier.
  private val competencyWithSkillTier =
    """{"result":{"framework":{"identifier":"fw_c","categories":[
         {"code":"competency","terms":[
            {"code":"area","children":[{"code":"leaf-a","name":"Leaf A"},{"code":"leaf-b","name":"Leaf B"}]}]},
         {"code":"skill","terms":[{"code":"not-a-leaf","name":"Interior"}]}]}}}"""

  "parseFlatSkills" should "take every term of a taxonomy framework's skill category" in {
    parseFlatSkills(usf) shouldBe Set("dv", "pp")
  }

  it should "not read industry or domain terms as skills" in {
    parseFlatSkills(usf) should not contain "it"
    parseFlatSkills(usf) should not contain "sd"
  }

  it should "defer to the competency tree when one exists" in {
    parseFlatSkills(competencyWithSkillTier) shouldBe Set.empty
  }

  it should "be empty for a framework with no skill category" in {
    parseFlatSkills("""{"result":{"framework":{"categories":[{"code":"board","terms":[{"code":"cbse"}]}]}}}""") shouldBe
      Set.empty
  }

  "parseTree" should "still find nothing in a taxonomy framework, so meta falls back to flat skills" in {
    parseTree(usf) shouldBe ((Set.empty[String], 0))
  }

  "parseLeafNames" should "map lower-cased leaf names to codes" in {
    parseLeafNames(usf, Set("dv", "pp")) shouldBe Map("data visualization" -> "dv", "python programming" -> "pp")
  }

  it should "read names off nested children, which carry no identifier" in {
    parseLeafNames(competencyWithSkillTier, Set("leaf-a", "leaf-b")) shouldBe
      Map("leaf a" -> "leaf-a", "leaf b" -> "leaf-b")
  }

  it should "leave out a name two leaves share" in {
    val dup =
      """{"result":{"framework":{"categories":[{"code":"skill","terms":[
           {"code":"x1","name":"Same"},{"code":"x2","name":"same"},{"code":"y","name":"Other"}]}]}}}"""
    parseLeafNames(dup, Set("x1", "x2", "y")) shouldBe Map("other" -> "y")
  }

  private val meta = CompetencyMeta("USF", Nil, Set("dv", "pp"), 1, Map.empty, Map.empty,
    codeByName = Map("data visualization" -> "dv"))

  "CompetencyMeta.leafOf" should "pass a leaf code through" in {
    meta.leafOf("dv") shouldBe Some("dv")
  }

  it should "resolve a name tag to its code, ignoring case and padding" in {
    meta.leafOf("Data Visualization") shouldBe Some("dv")
    meta.leafOf("  data VISUALIZATION ") shouldBe Some("dv")
  }

  it should "reject a tag that is neither a leaf code nor a leaf name" in {
    meta.leafOf("Cloud Computing") shouldBe None
    meta.leafOf(null) shouldBe None
  }
}
