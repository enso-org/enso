import static org.junit.Assert.fail;

import org.junit.Test;

public class EpbLanguageDependenciesTest {
  public EpbLanguageDependenciesTest() {}

  @Test(expected = ClassNotFoundException.class)
  public void avoidFlatbuffers() throws Exception {
    var c = Class.forName("com.google.flatbuffers.Constants");
    fail("No class should be found: " + c);
  }

  @Test(expected = ClassNotFoundException.class)
  public void avoidJackson() throws Exception {
    var c = Class.forName("com.fasterxml.jackson.core.JsonParser");
    fail("No class should be found: " + c);
  }

  @Test(expected = ClassNotFoundException.class)
  public void avoidCirce() throws Exception {
    var c = Class.forName("io.circe.Codec");
    fail("No class should be found: " + c);
  }

  @Test(expected = ClassNotFoundException.class)
  public void avoidApacheCommons() throws Exception {
    var c = Class.forName("org.apache.commons.io.Charsets");
    fail("No class should be found: " + c);
  }

  @Test(expected = ClassNotFoundException.class)
  public void avoidCats() throws Exception {
    var c = Class.forName("cats.Align");
    fail("No class should be found: " + c);
  }

  @Test(expected = ClassNotFoundException.class)
  public void avoidSlf4j() throws Exception {
    var c = Class.forName("org.slf4j.Logger");
    fail("No class should be found: " + c);
  }

  @Test(expected = ClassNotFoundException.class)
  public void avoidSnakeyaml() throws Exception {
    var c = Class.forName("org.yaml.snakeyaml.Yaml");
    fail("No class should be found: " + c);
  }
}
