// Generated from C:/Repos/Enso/Epressions/src/org/enso/table/parsing\Expression.g4 by ANTLR 4.10.1
package org.enso.table.expressions;

import org.antlr.v4.runtime.tree.ParseTreeVisitor;

/**
 * This interface defines a complete generic visitor for a parse tree produced by {@link
 * ExpressionParser}.
 *
 * @param <T> The return type of the visit operation. Use {@link Void} for operations with no return
 *     type.
 */
public interface ExpressionVisitor<T> extends ParseTreeVisitor<T> {
  /**
   * Visit a parse tree produced by {@link ExpressionParser#prog}.
   *
   * @param ctx the parse tree
   * @return the visitor result
   */
  T visitProg(ExpressionParser.ProgContext ctx);
  /**
   * Visit a parse tree produced by the {@code Or} labeled alternative in {@link
   * ExpressionParser#expr}.
   *
   * @param ctx the parse tree
   * @return the visitor result
   */
  T visitOr(ExpressionParser.OrContext ctx);
  /**
   * Visit a parse tree produced by the {@code AddSub} labeled alternative in {@link
   * ExpressionParser#expr}.
   *
   * @param ctx the parse tree
   * @return the visitor result
   */
  T visitAddSub(ExpressionParser.AddSubContext ctx);
  /**
   * Visit a parse tree produced by the {@code In} labeled alternative in {@link
   * ExpressionParser#expr}.
   *
   * @param ctx the parse tree
   * @return the visitor result
   */
  T visitIn(ExpressionParser.InContext ctx);
  /**
   * Visit a parse tree produced by the {@code Between} labeled alternative in {@link
   * ExpressionParser#expr}.
   *
   * @param ctx the parse tree
   * @return the visitor result
   */
  T visitBetween(ExpressionParser.BetweenContext ctx);
  /**
   * Visit a parse tree produced by the {@code UnaryMinus} labeled alternative in {@link
   * ExpressionParser#expr}.
   *
   * @param ctx the parse tree
   * @return the visitor result
   */
  T visitUnaryMinus(ExpressionParser.UnaryMinusContext ctx);
  /**
   * Visit a parse tree produced by the {@code Function} labeled alternative in {@link
   * ExpressionParser#expr}.
   *
   * @param ctx the parse tree
   * @return the visitor result
   */
  T visitFunction(ExpressionParser.FunctionContext ctx);
  /**
   * Visit a parse tree produced by the {@code UnaryNot} labeled alternative in {@link
   * ExpressionParser#expr}.
   *
   * @param ctx the parse tree
   * @return the visitor result
   */
  T visitUnaryNot(ExpressionParser.UnaryNotContext ctx);
  /**
   * Visit a parse tree produced by the {@code Like} labeled alternative in {@link
   * ExpressionParser#expr}.
   *
   * @param ctx the parse tree
   * @return the visitor result
   */
  T visitLike(ExpressionParser.LikeContext ctx);
  /**
   * Visit a parse tree produced by the {@code Column} labeled alternative in {@link
   * ExpressionParser#expr}.
   *
   * @param ctx the parse tree
   * @return the visitor result
   */
  T visitColumn(ExpressionParser.ColumnContext ctx);
  /**
   * Visit a parse tree produced by the {@code MultDivMod} labeled alternative in {@link
   * ExpressionParser#expr}.
   *
   * @param ctx the parse tree
   * @return the visitor result
   */
  T visitMultDivMod(ExpressionParser.MultDivModContext ctx);
  /**
   * Visit a parse tree produced by the {@code And} labeled alternative in {@link
   * ExpressionParser#expr}.
   *
   * @param ctx the parse tree
   * @return the visitor result
   */
  T visitAnd(ExpressionParser.AndContext ctx);
  /**
   * Visit a parse tree produced by the {@code Literal} labeled alternative in {@link
   * ExpressionParser#expr}.
   *
   * @param ctx the parse tree
   * @return the visitor result
   */
  T visitLiteral(ExpressionParser.LiteralContext ctx);
  /**
   * Visit a parse tree produced by the {@code Compare} labeled alternative in {@link
   * ExpressionParser#expr}.
   *
   * @param ctx the parse tree
   * @return the visitor result
   */
  T visitCompare(ExpressionParser.CompareContext ctx);
  /**
   * Visit a parse tree produced by the {@code IsNull} labeled alternative in {@link
   * ExpressionParser#expr}.
   *
   * @param ctx the parse tree
   * @return the visitor result
   */
  T visitIsNull(ExpressionParser.IsNullContext ctx);
  /**
   * Visit a parse tree produced by the {@code Power} labeled alternative in {@link
   * ExpressionParser#expr}.
   *
   * @param ctx the parse tree
   * @return the visitor result
   */
  T visitPower(ExpressionParser.PowerContext ctx);
  /**
   * Visit a parse tree produced by the {@code Paren} labeled alternative in {@link
   * ExpressionParser#expr}.
   *
   * @param ctx the parse tree
   * @return the visitor result
   */
  T visitParen(ExpressionParser.ParenContext ctx);
  /**
   * Visit a parse tree produced by the {@code nullOrNothing} labeled alternative in {@link
   * ExpressionParser#value}.
   *
   * @param ctx the parse tree
   * @return the visitor result
   */
  T visitNullOrNothing(ExpressionParser.NullOrNothingContext ctx);
  /**
   * Visit a parse tree produced by the {@code boolean} labeled alternative in {@link
   * ExpressionParser#value}.
   *
   * @param ctx the parse tree
   * @return the visitor result
   */
  T visitBoolean(ExpressionParser.BooleanContext ctx);
  /**
   * Visit a parse tree produced by the {@code date} labeled alternative in {@link
   * ExpressionParser#value}.
   *
   * @param ctx the parse tree
   * @return the visitor result
   */
  T visitDate(ExpressionParser.DateContext ctx);
  /**
   * Visit a parse tree produced by the {@code time} labeled alternative in {@link
   * ExpressionParser#value}.
   *
   * @param ctx the parse tree
   * @return the visitor result
   */
  T visitTime(ExpressionParser.TimeContext ctx);
  /**
   * Visit a parse tree produced by the {@code datetime} labeled alternative in {@link
   * ExpressionParser#value}.
   *
   * @param ctx the parse tree
   * @return the visitor result
   */
  T visitDatetime(ExpressionParser.DatetimeContext ctx);
  /**
   * Visit a parse tree produced by the {@code number} labeled alternative in {@link
   * ExpressionParser#value}.
   *
   * @param ctx the parse tree
   * @return the visitor result
   */
  T visitNumber(ExpressionParser.NumberContext ctx);
  /**
   * Visit a parse tree produced by the {@code excelString} labeled alternative in {@link
   * ExpressionParser#value}.
   *
   * @param ctx the parse tree
   * @return the visitor result
   */
  T visitExcelString(ExpressionParser.ExcelStringContext ctx);
  /**
   * Visit a parse tree produced by the {@code pythonString} labeled alternative in {@link
   * ExpressionParser#value}.
   *
   * @param ctx the parse tree
   * @return the visitor result
   */
  T visitPythonString(ExpressionParser.PythonStringContext ctx);
}
