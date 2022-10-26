// Generated from C:/Repos/Enso/ide/std-bits/table/src/main/antlr4\Expression.g4 by ANTLR 4.10.1
package org.enso.table.expressions;

import org.antlr.v4.runtime.tree.ParseTreeListener;

/**
 * This interface defines a complete listener for a parse tree produced by {@link ExpressionParser}.
 */
public interface ExpressionListener extends ParseTreeListener {
  /**
   * Enter a parse tree produced by {@link ExpressionParser#prog}.
   *
   * @param ctx the parse tree
   */
  void enterProg(ExpressionParser.ProgContext ctx);
  /**
   * Exit a parse tree produced by {@link ExpressionParser#prog}.
   *
   * @param ctx the parse tree
   */
  void exitProg(ExpressionParser.ProgContext ctx);
  /**
   * Enter a parse tree produced by the {@code Or} labeled alternative in {@link
   * ExpressionParser#expr}.
   *
   * @param ctx the parse tree
   */
  void enterOr(ExpressionParser.OrContext ctx);
  /**
   * Exit a parse tree produced by the {@code Or} labeled alternative in {@link
   * ExpressionParser#expr}.
   *
   * @param ctx the parse tree
   */
  void exitOr(ExpressionParser.OrContext ctx);
  /**
   * Enter a parse tree produced by the {@code AddSub} labeled alternative in {@link
   * ExpressionParser#expr}.
   *
   * @param ctx the parse tree
   */
  void enterAddSub(ExpressionParser.AddSubContext ctx);
  /**
   * Exit a parse tree produced by the {@code AddSub} labeled alternative in {@link
   * ExpressionParser#expr}.
   *
   * @param ctx the parse tree
   */
  void exitAddSub(ExpressionParser.AddSubContext ctx);
  /**
   * Enter a parse tree produced by the {@code In} labeled alternative in {@link
   * ExpressionParser#expr}.
   *
   * @param ctx the parse tree
   */
  void enterIn(ExpressionParser.InContext ctx);
  /**
   * Exit a parse tree produced by the {@code In} labeled alternative in {@link
   * ExpressionParser#expr}.
   *
   * @param ctx the parse tree
   */
  void exitIn(ExpressionParser.InContext ctx);
  /**
   * Enter a parse tree produced by the {@code Between} labeled alternative in {@link
   * ExpressionParser#expr}.
   *
   * @param ctx the parse tree
   */
  void enterBetween(ExpressionParser.BetweenContext ctx);
  /**
   * Exit a parse tree produced by the {@code Between} labeled alternative in {@link
   * ExpressionParser#expr}.
   *
   * @param ctx the parse tree
   */
  void exitBetween(ExpressionParser.BetweenContext ctx);
  /**
   * Enter a parse tree produced by the {@code UnaryMinus} labeled alternative in {@link
   * ExpressionParser#expr}.
   *
   * @param ctx the parse tree
   */
  void enterUnaryMinus(ExpressionParser.UnaryMinusContext ctx);
  /**
   * Exit a parse tree produced by the {@code UnaryMinus} labeled alternative in {@link
   * ExpressionParser#expr}.
   *
   * @param ctx the parse tree
   */
  void exitUnaryMinus(ExpressionParser.UnaryMinusContext ctx);
  /**
   * Enter a parse tree produced by the {@code Function} labeled alternative in {@link
   * ExpressionParser#expr}.
   *
   * @param ctx the parse tree
   */
  void enterFunction(ExpressionParser.FunctionContext ctx);
  /**
   * Exit a parse tree produced by the {@code Function} labeled alternative in {@link
   * ExpressionParser#expr}.
   *
   * @param ctx the parse tree
   */
  void exitFunction(ExpressionParser.FunctionContext ctx);
  /**
   * Enter a parse tree produced by the {@code UnaryNot} labeled alternative in {@link
   * ExpressionParser#expr}.
   *
   * @param ctx the parse tree
   */
  void enterUnaryNot(ExpressionParser.UnaryNotContext ctx);
  /**
   * Exit a parse tree produced by the {@code UnaryNot} labeled alternative in {@link
   * ExpressionParser#expr}.
   *
   * @param ctx the parse tree
   */
  void exitUnaryNot(ExpressionParser.UnaryNotContext ctx);
  /**
   * Enter a parse tree produced by the {@code Like} labeled alternative in {@link
   * ExpressionParser#expr}.
   *
   * @param ctx the parse tree
   */
  void enterLike(ExpressionParser.LikeContext ctx);
  /**
   * Exit a parse tree produced by the {@code Like} labeled alternative in {@link
   * ExpressionParser#expr}.
   *
   * @param ctx the parse tree
   */
  void exitLike(ExpressionParser.LikeContext ctx);
  /**
   * Enter a parse tree produced by the {@code Column} labeled alternative in {@link
   * ExpressionParser#expr}.
   *
   * @param ctx the parse tree
   */
  void enterColumn(ExpressionParser.ColumnContext ctx);
  /**
   * Exit a parse tree produced by the {@code Column} labeled alternative in {@link
   * ExpressionParser#expr}.
   *
   * @param ctx the parse tree
   */
  void exitColumn(ExpressionParser.ColumnContext ctx);
  /**
   * Enter a parse tree produced by the {@code MultDivMod} labeled alternative in {@link
   * ExpressionParser#expr}.
   *
   * @param ctx the parse tree
   */
  void enterMultDivMod(ExpressionParser.MultDivModContext ctx);
  /**
   * Exit a parse tree produced by the {@code MultDivMod} labeled alternative in {@link
   * ExpressionParser#expr}.
   *
   * @param ctx the parse tree
   */
  void exitMultDivMod(ExpressionParser.MultDivModContext ctx);
  /**
   * Enter a parse tree produced by the {@code And} labeled alternative in {@link
   * ExpressionParser#expr}.
   *
   * @param ctx the parse tree
   */
  void enterAnd(ExpressionParser.AndContext ctx);
  /**
   * Exit a parse tree produced by the {@code And} labeled alternative in {@link
   * ExpressionParser#expr}.
   *
   * @param ctx the parse tree
   */
  void exitAnd(ExpressionParser.AndContext ctx);
  /**
   * Enter a parse tree produced by the {@code Literal} labeled alternative in {@link
   * ExpressionParser#expr}.
   *
   * @param ctx the parse tree
   */
  void enterLiteral(ExpressionParser.LiteralContext ctx);
  /**
   * Exit a parse tree produced by the {@code Literal} labeled alternative in {@link
   * ExpressionParser#expr}.
   *
   * @param ctx the parse tree
   */
  void exitLiteral(ExpressionParser.LiteralContext ctx);
  /**
   * Enter a parse tree produced by the {@code Compare} labeled alternative in {@link
   * ExpressionParser#expr}.
   *
   * @param ctx the parse tree
   */
  void enterCompare(ExpressionParser.CompareContext ctx);
  /**
   * Exit a parse tree produced by the {@code Compare} labeled alternative in {@link
   * ExpressionParser#expr}.
   *
   * @param ctx the parse tree
   */
  void exitCompare(ExpressionParser.CompareContext ctx);
  /**
   * Enter a parse tree produced by the {@code IsNull} labeled alternative in {@link
   * ExpressionParser#expr}.
   *
   * @param ctx the parse tree
   */
  void enterIsNull(ExpressionParser.IsNullContext ctx);
  /**
   * Exit a parse tree produced by the {@code IsNull} labeled alternative in {@link
   * ExpressionParser#expr}.
   *
   * @param ctx the parse tree
   */
  void exitIsNull(ExpressionParser.IsNullContext ctx);
  /**
   * Enter a parse tree produced by the {@code If} labeled alternative in {@link
   * ExpressionParser#expr}.
   *
   * @param ctx the parse tree
   */
  void enterIf(ExpressionParser.IfContext ctx);
  /**
   * Exit a parse tree produced by the {@code If} labeled alternative in {@link
   * ExpressionParser#expr}.
   *
   * @param ctx the parse tree
   */
  void exitIf(ExpressionParser.IfContext ctx);
  /**
   * Enter a parse tree produced by the {@code Power} labeled alternative in {@link
   * ExpressionParser#expr}.
   *
   * @param ctx the parse tree
   */
  void enterPower(ExpressionParser.PowerContext ctx);
  /**
   * Exit a parse tree produced by the {@code Power} labeled alternative in {@link
   * ExpressionParser#expr}.
   *
   * @param ctx the parse tree
   */
  void exitPower(ExpressionParser.PowerContext ctx);
  /**
   * Enter a parse tree produced by the {@code Paren} labeled alternative in {@link
   * ExpressionParser#expr}.
   *
   * @param ctx the parse tree
   */
  void enterParen(ExpressionParser.ParenContext ctx);
  /**
   * Exit a parse tree produced by the {@code Paren} labeled alternative in {@link
   * ExpressionParser#expr}.
   *
   * @param ctx the parse tree
   */
  void exitParen(ExpressionParser.ParenContext ctx);
  /**
   * Enter a parse tree produced by the {@code nullOrNothing} labeled alternative in {@link
   * ExpressionParser#value}.
   *
   * @param ctx the parse tree
   */
  void enterNullOrNothing(ExpressionParser.NullOrNothingContext ctx);
  /**
   * Exit a parse tree produced by the {@code nullOrNothing} labeled alternative in {@link
   * ExpressionParser#value}.
   *
   * @param ctx the parse tree
   */
  void exitNullOrNothing(ExpressionParser.NullOrNothingContext ctx);
  /**
   * Enter a parse tree produced by the {@code boolean} labeled alternative in {@link
   * ExpressionParser#value}.
   *
   * @param ctx the parse tree
   */
  void enterBoolean(ExpressionParser.BooleanContext ctx);
  /**
   * Exit a parse tree produced by the {@code boolean} labeled alternative in {@link
   * ExpressionParser#value}.
   *
   * @param ctx the parse tree
   */
  void exitBoolean(ExpressionParser.BooleanContext ctx);
  /**
   * Enter a parse tree produced by the {@code date} labeled alternative in {@link
   * ExpressionParser#value}.
   *
   * @param ctx the parse tree
   */
  void enterDate(ExpressionParser.DateContext ctx);
  /**
   * Exit a parse tree produced by the {@code date} labeled alternative in {@link
   * ExpressionParser#value}.
   *
   * @param ctx the parse tree
   */
  void exitDate(ExpressionParser.DateContext ctx);
  /**
   * Enter a parse tree produced by the {@code time} labeled alternative in {@link
   * ExpressionParser#value}.
   *
   * @param ctx the parse tree
   */
  void enterTime(ExpressionParser.TimeContext ctx);
  /**
   * Exit a parse tree produced by the {@code time} labeled alternative in {@link
   * ExpressionParser#value}.
   *
   * @param ctx the parse tree
   */
  void exitTime(ExpressionParser.TimeContext ctx);
  /**
   * Enter a parse tree produced by the {@code datetime} labeled alternative in {@link
   * ExpressionParser#value}.
   *
   * @param ctx the parse tree
   */
  void enterDatetime(ExpressionParser.DatetimeContext ctx);
  /**
   * Exit a parse tree produced by the {@code datetime} labeled alternative in {@link
   * ExpressionParser#value}.
   *
   * @param ctx the parse tree
   */
  void exitDatetime(ExpressionParser.DatetimeContext ctx);
  /**
   * Enter a parse tree produced by the {@code number} labeled alternative in {@link
   * ExpressionParser#value}.
   *
   * @param ctx the parse tree
   */
  void enterNumber(ExpressionParser.NumberContext ctx);
  /**
   * Exit a parse tree produced by the {@code number} labeled alternative in {@link
   * ExpressionParser#value}.
   *
   * @param ctx the parse tree
   */
  void exitNumber(ExpressionParser.NumberContext ctx);
  /**
   * Enter a parse tree produced by the {@code excelString} labeled alternative in {@link
   * ExpressionParser#value}.
   *
   * @param ctx the parse tree
   */
  void enterExcelString(ExpressionParser.ExcelStringContext ctx);
  /**
   * Exit a parse tree produced by the {@code excelString} labeled alternative in {@link
   * ExpressionParser#value}.
   *
   * @param ctx the parse tree
   */
  void exitExcelString(ExpressionParser.ExcelStringContext ctx);
  /**
   * Enter a parse tree produced by the {@code pythonString} labeled alternative in {@link
   * ExpressionParser#value}.
   *
   * @param ctx the parse tree
   */
  void enterPythonString(ExpressionParser.PythonStringContext ctx);
  /**
   * Exit a parse tree produced by the {@code pythonString} labeled alternative in {@link
   * ExpressionParser#value}.
   *
   * @param ctx the parse tree
   */
  void exitPythonString(ExpressionParser.PythonStringContext ctx);
}
