package org.enso.refactoring;

import org.enso.syntax.text.Location;
import org.enso.text.buffer.Rope;
import org.enso.text.editing.IndexedSource;
import org.enso.text.editing.IndexedSource$;
import org.enso.text.editing.JavaEditorAdapter;
import org.enso.text.editing.TextEditValidationFailure;
import org.enso.text.editing.model;
import org.junit.Assert;
import org.junit.Test;
import scala.collection.immutable.Seq;
import scala.collection.immutable.VectorBuilder;
import scala.util.Either;

public class RenameUtilsTest {

  private final IndexedSource<Rope> indexedSource;

  public RenameUtilsTest() {
    indexedSource = IndexedSource$.MODULE$.RopeIndexedSource();
  }

  @Test
  public void buildEdits() {
    Rope source = Rope.apply("foo bar foo baz");

    String newName = "quux";
    Seq<Location> occurrences = vec(new Location(0, 3), new Location(8, 11));
    Seq<model.TextEdit> edits =
        RenameUtils$.MODULE$.buildEdits(source, occurrences, newName, indexedSource);

    Assert.assertEquals(2, edits.length());

    Either<TextEditValidationFailure, Rope> result = JavaEditorAdapter.applyEdits(source, edits);
    Assert.assertTrue(result.isRight());

    Rope expected = Rope.apply("quux bar quux baz");
    Assert.assertEquals(expected.toString(), result.toOption().get().toString());
  }

  @Test
  public void buildEditsUnordered() {
    Rope source = Rope.apply("foo bar foo baz");

    String newName = "quux";
    Seq<Location> occurrences = vec(new Location(8, 11), new Location(0, 3));
    Seq<model.TextEdit> edits =
        RenameUtils$.MODULE$.buildEdits(source, occurrences, newName, indexedSource);

    Assert.assertEquals(2, edits.length());

    Either<TextEditValidationFailure, Rope> result = JavaEditorAdapter.applyEdits(source, edits);
    Assert.assertTrue(result.isRight());

    Rope expected = Rope.apply("quux bar quux baz");
    Assert.assertEquals(expected.toString(), result.toOption().get().toString());
  }

  @Test
  public void buildEditsEmptyOccurrences() {
    Rope source = Rope.apply("foo bar foo baz");

    String newName = "quux";
    Seq<Location> occurrences = vec();
    Seq<model.TextEdit> edits =
        RenameUtils$.MODULE$.buildEdits(source, occurrences, newName, indexedSource);

    Assert.assertEquals(0, edits.length());

    Either<TextEditValidationFailure, Rope> result = JavaEditorAdapter.applyEdits(source, edits);
    Assert.assertTrue(result.isRight());
    Assert.assertEquals(source.toString(), result.toOption().get().toString());
  }

  @Test
  public void buildEditsRemovingOccurrences() {
    Rope source = Rope.apply("foo bar foo baz");

    String newName = "";
    Seq<Location> occurrences = vec(new Location(0, 3), new Location(8, 11));
    Seq<model.TextEdit> edits =
        RenameUtils$.MODULE$.buildEdits(source, occurrences, newName, indexedSource);

    Assert.assertEquals(2, edits.length());

    Either<TextEditValidationFailure, Rope> result = JavaEditorAdapter.applyEdits(source, edits);
    Assert.assertTrue(result.isRight());

    Rope expected = Rope.apply(" bar  baz");
    Assert.assertEquals(expected.toString(), result.toOption().get().toString());
  }

  @SafeVarargs
  private static <A> Seq<A> vec(A... elems) {
    VectorBuilder<A> builder = new VectorBuilder<>();

    for (A elem : elems) {
      builder.addOne(elem);
    }

    return builder.result();
  }
}
