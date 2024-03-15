package org.enso.tools.enso4igv;

import java.util.List;
import static org.junit.Assert.assertEquals;
import org.junit.BeforeClass;
import org.junit.Test;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import org.netbeans.api.lexer.Language;
import org.netbeans.api.lexer.TokenId;
import org.netbeans.api.lexer.TokenSequence;

public class EnsoTextMateTest {

    private static Language<? extends TokenId> ensoLanguage;

    @BeforeClass
    public static void findEnsoLanguage() {
        ensoLanguage = org.netbeans.api.lexer.Language.find("application/x-enso");
        assertNotNull("Needs org-netbeans-modules-lexer-nbbridge dependency", ensoLanguage);
    }

    @Test
    public void testSimpleMain() {
        var code = """
                  main = 42
                  """;
        var hier = org.netbeans.api.lexer.TokenHierarchy.create(code, ensoLanguage);
        assertNotNull(hier);
        var seq = hier.tokenSequence();
        assertEquals(6, seq.tokenCount());
        assertNextToken(seq, "main", "entity.name.function");
        assertNextToken(seq, " ", null);
        assertNextToken(seq, "=", "keyword.operator");
        assertNextToken(seq, " ", null);
        assertNextToken(seq, "42", "constant.character.numeric");
    }

    private static void assertNextToken(TokenSequence<?> seq, String expectedText, String expectedCategory) {
        assertTrue("Can move next", seq.moveNext());
        var tok = seq.token();
        assertNotNull("Token found", tok);
        if (expectedText != null) {
            assertEquals(expectedText, tok.text().toString());
        }
        if (expectedCategory != null) {
            var categories = (List<String>) tok.getProperty("categories");
            var at = categories.indexOf(expectedCategory);
            assertNotEquals("Category " + expectedCategory + " found in " + categories, -1, at);
        }
    }
}
