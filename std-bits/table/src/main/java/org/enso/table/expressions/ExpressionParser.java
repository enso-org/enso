// Generated from C:/Repos/Enso/ide/std-bits/table/src/main/antlr4\Expression.g4 by ANTLR 4.10.1
package org.enso.table.expressions;

import org.antlr.v4.runtime.atn.*;
import org.antlr.v4.runtime.dfa.DFA;
import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.tree.*;
import java.util.List;

@SuppressWarnings({"all", "warnings", "unchecked", "unused", "cast"})
public class ExpressionParser extends Parser {
  static {
    RuntimeMetaData.checkVersion("4.10.1", RuntimeMetaData.VERSION);
  }

  protected static final DFA[] _decisionToDFA;
  protected static final PredictionContextCache _sharedContextCache = new PredictionContextCache();
  public static final int T__0 = 1,
      T__1 = 2,
      T__2 = 3,
      T__3 = 4,
      POWER = 5,
      MULTIPLY = 6,
      DIVIDE = 7,
      MODULO = 8,
      ADD = 9,
      MINUS = 10,
      EQUALS = 11,
      NOT_EQUALS = 12,
      LESS_THAN_OR_EQUAL = 13,
      GREATER_THAN_OR_EQUAL = 14,
      LESS_THAN = 15,
      GREATER_THAN = 16,
      WHITESPACE = 17,
      AND = 18,
      OR = 19,
      NULL = 20,
      NOTHING = 21,
      IS_NULL = 22,
      IS_NOT_NULL = 23,
      IS_EMPTY = 24,
      IS_NOT_EMPTY = 25,
      LIKE = 26,
      NOT_LIKE = 27,
      IN = 28,
      BETWEEN = 29,
      NOT_BETWEEN = 30,
      TRUE = 31,
      FALSE = 32,
      IF = 33,
      THEN = 34,
      ELSE = 35,
      UNARY_NOT = 36,
      END = 37,
      IDENTIFIER = 38,
      EXCEL_STRING = 39,
      PYTHON_STRING = 40,
      DATE = 41,
      TIME = 42,
      DATE_TIME = 43,
      NUMBER = 44,
      COLUMN_NAME = 45;
  public static final int RULE_prog = 0, RULE_expr = 1, RULE_value = 2;

  private static String[] makeRuleNames() {
    return new String[] {"prog", "expr", "value"};
  }

  public static final String[] ruleNames = makeRuleNames();

  private static String[] makeLiteralNames() {
    return new String[] {
      null, "'('", "','", "')'", "'#'", "'^'", "'*'", "'/'", "'%'", "'+'", "'-'", null, null,
      "'<='", "'>='", "'<'", "'>'"
    };
  }

  private static final String[] _LITERAL_NAMES = makeLiteralNames();

  private static String[] makeSymbolicNames() {
    return new String[] {
      null,
      null,
      null,
      null,
      null,
      "POWER",
      "MULTIPLY",
      "DIVIDE",
      "MODULO",
      "ADD",
      "MINUS",
      "EQUALS",
      "NOT_EQUALS",
      "LESS_THAN_OR_EQUAL",
      "GREATER_THAN_OR_EQUAL",
      "LESS_THAN",
      "GREATER_THAN",
      "WHITESPACE",
      "AND",
      "OR",
      "NULL",
      "NOTHING",
      "IS_NULL",
      "IS_NOT_NULL",
      "IS_EMPTY",
      "IS_NOT_EMPTY",
      "LIKE",
      "NOT_LIKE",
      "IN",
      "BETWEEN",
      "NOT_BETWEEN",
      "TRUE",
      "FALSE",
      "IF",
      "THEN",
      "ELSE",
      "UNARY_NOT",
      "END",
      "IDENTIFIER",
      "EXCEL_STRING",
      "PYTHON_STRING",
      "DATE",
      "TIME",
      "DATE_TIME",
      "NUMBER",
      "COLUMN_NAME"
    };
  }

  private static final String[] _SYMBOLIC_NAMES = makeSymbolicNames();
  public static final Vocabulary VOCABULARY = new VocabularyImpl(_LITERAL_NAMES, _SYMBOLIC_NAMES);

  /** @deprecated Use {@link #VOCABULARY} instead. */
  @Deprecated public static final String[] tokenNames;

  static {
    tokenNames = new String[_SYMBOLIC_NAMES.length];
    for (int i = 0; i < tokenNames.length; i++) {
      tokenNames[i] = VOCABULARY.getLiteralName(i);
      if (tokenNames[i] == null) {
        tokenNames[i] = VOCABULARY.getSymbolicName(i);
      }

      if (tokenNames[i] == null) {
        tokenNames[i] = "<INVALID>";
      }
    }
  }

  @Override
  @Deprecated
  public String[] getTokenNames() {
    return tokenNames;
  }

  @Override
  public Vocabulary getVocabulary() {
    return VOCABULARY;
  }

  @Override
  public String getGrammarFileName() {
    return "Expression.g4";
  }

  @Override
  public String[] getRuleNames() {
    return ruleNames;
  }

  @Override
  public String getSerializedATN() {
    return _serializedATN;
  }

  @Override
  public ATN getATN() {
    return _ATN;
  }

  public ExpressionParser(TokenStream input) {
    super(input);
    _interp = new ParserATNSimulator(this, _ATN, _decisionToDFA, _sharedContextCache);
  }

  public static class ProgContext extends ParserRuleContext {
    public ExprContext expr() {
      return getRuleContext(ExprContext.class, 0);
    }

    public TerminalNode EOF() {
      return getToken(ExpressionParser.EOF, 0);
    }

    public ProgContext(ParserRuleContext parent, int invokingState) {
      super(parent, invokingState);
    }

    @Override
    public int getRuleIndex() {
      return RULE_prog;
    }

    @Override
    public void enterRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener) ((ExpressionListener) listener).enterProg(this);
    }

    @Override
    public void exitRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener) ((ExpressionListener) listener).exitProg(this);
    }

    @Override
    public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
      if (visitor instanceof ExpressionVisitor)
        return ((ExpressionVisitor<? extends T>) visitor).visitProg(this);
      else return visitor.visitChildren(this);
    }
  }

  public final ProgContext prog() throws RecognitionException {
    ProgContext _localctx = new ProgContext(_ctx, getState());
    enterRule(_localctx, 0, RULE_prog);
    try {
      enterOuterAlt(_localctx, 1);
      {
        setState(6);
        expr(0);
        setState(7);
        match(EOF);
      }
    } catch (RecognitionException re) {
      _localctx.exception = re;
      _errHandler.reportError(this, re);
      _errHandler.recover(this, re);
    } finally {
      exitRule();
    }
    return _localctx;
  }

  public static class ExprContext extends ParserRuleContext {
    public ExprContext(ParserRuleContext parent, int invokingState) {
      super(parent, invokingState);
    }

    @Override
    public int getRuleIndex() {
      return RULE_expr;
    }

    public ExprContext() {}

    public void copyFrom(ExprContext ctx) {
      super.copyFrom(ctx);
    }
  }

  public static class OrContext extends ExprContext {
    public Token op;

    public List<ExprContext> expr() {
      return getRuleContexts(ExprContext.class);
    }

    public ExprContext expr(int i) {
      return getRuleContext(ExprContext.class, i);
    }

    public TerminalNode OR() {
      return getToken(ExpressionParser.OR, 0);
    }

    public OrContext(ExprContext ctx) {
      copyFrom(ctx);
    }

    @Override
    public void enterRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener) ((ExpressionListener) listener).enterOr(this);
    }

    @Override
    public void exitRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener) ((ExpressionListener) listener).exitOr(this);
    }

    @Override
    public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
      if (visitor instanceof ExpressionVisitor)
        return ((ExpressionVisitor<? extends T>) visitor).visitOr(this);
      else return visitor.visitChildren(this);
    }
  }

  public static class AddSubContext extends ExprContext {
    public Token op;

    public List<ExprContext> expr() {
      return getRuleContexts(ExprContext.class);
    }

    public ExprContext expr(int i) {
      return getRuleContext(ExprContext.class, i);
    }

    public TerminalNode ADD() {
      return getToken(ExpressionParser.ADD, 0);
    }

    public TerminalNode MINUS() {
      return getToken(ExpressionParser.MINUS, 0);
    }

    public AddSubContext(ExprContext ctx) {
      copyFrom(ctx);
    }

    @Override
    public void enterRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener) ((ExpressionListener) listener).enterAddSub(this);
    }

    @Override
    public void exitRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener) ((ExpressionListener) listener).exitAddSub(this);
    }

    @Override
    public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
      if (visitor instanceof ExpressionVisitor)
        return ((ExpressionVisitor<? extends T>) visitor).visitAddSub(this);
      else return visitor.visitChildren(this);
    }
  }

  public static class InContext extends ExprContext {
    public List<ExprContext> expr() {
      return getRuleContexts(ExprContext.class);
    }

    public ExprContext expr(int i) {
      return getRuleContext(ExprContext.class, i);
    }

    public TerminalNode IN() {
      return getToken(ExpressionParser.IN, 0);
    }

    public InContext(ExprContext ctx) {
      copyFrom(ctx);
    }

    @Override
    public void enterRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener) ((ExpressionListener) listener).enterIn(this);
    }

    @Override
    public void exitRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener) ((ExpressionListener) listener).exitIn(this);
    }

    @Override
    public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
      if (visitor instanceof ExpressionVisitor)
        return ((ExpressionVisitor<? extends T>) visitor).visitIn(this);
      else return visitor.visitChildren(this);
    }
  }

  public static class BetweenContext extends ExprContext {
    public List<ExprContext> expr() {
      return getRuleContexts(ExprContext.class);
    }

    public ExprContext expr(int i) {
      return getRuleContext(ExprContext.class, i);
    }

    public TerminalNode AND() {
      return getToken(ExpressionParser.AND, 0);
    }

    public TerminalNode NOT_BETWEEN() {
      return getToken(ExpressionParser.NOT_BETWEEN, 0);
    }

    public TerminalNode BETWEEN() {
      return getToken(ExpressionParser.BETWEEN, 0);
    }

    public BetweenContext(ExprContext ctx) {
      copyFrom(ctx);
    }

    @Override
    public void enterRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener)
        ((ExpressionListener) listener).enterBetween(this);
    }

    @Override
    public void exitRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener) ((ExpressionListener) listener).exitBetween(this);
    }

    @Override
    public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
      if (visitor instanceof ExpressionVisitor)
        return ((ExpressionVisitor<? extends T>) visitor).visitBetween(this);
      else return visitor.visitChildren(this);
    }
  }

  public static class UnaryMinusContext extends ExprContext {
    public TerminalNode MINUS() {
      return getToken(ExpressionParser.MINUS, 0);
    }

    public ExprContext expr() {
      return getRuleContext(ExprContext.class, 0);
    }

    public UnaryMinusContext(ExprContext ctx) {
      copyFrom(ctx);
    }

    @Override
    public void enterRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener)
        ((ExpressionListener) listener).enterUnaryMinus(this);
    }

    @Override
    public void exitRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener)
        ((ExpressionListener) listener).exitUnaryMinus(this);
    }

    @Override
    public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
      if (visitor instanceof ExpressionVisitor)
        return ((ExpressionVisitor<? extends T>) visitor).visitUnaryMinus(this);
      else return visitor.visitChildren(this);
    }
  }

  public static class FunctionContext extends ExprContext {
    public TerminalNode IDENTIFIER() {
      return getToken(ExpressionParser.IDENTIFIER, 0);
    }

    public List<ExprContext> expr() {
      return getRuleContexts(ExprContext.class);
    }

    public ExprContext expr(int i) {
      return getRuleContext(ExprContext.class, i);
    }

    public FunctionContext(ExprContext ctx) {
      copyFrom(ctx);
    }

    @Override
    public void enterRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener)
        ((ExpressionListener) listener).enterFunction(this);
    }

    @Override
    public void exitRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener)
        ((ExpressionListener) listener).exitFunction(this);
    }

    @Override
    public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
      if (visitor instanceof ExpressionVisitor)
        return ((ExpressionVisitor<? extends T>) visitor).visitFunction(this);
      else return visitor.visitChildren(this);
    }
  }

  public static class UnaryNotContext extends ExprContext {
    public TerminalNode UNARY_NOT() {
      return getToken(ExpressionParser.UNARY_NOT, 0);
    }

    public ExprContext expr() {
      return getRuleContext(ExprContext.class, 0);
    }

    public UnaryNotContext(ExprContext ctx) {
      copyFrom(ctx);
    }

    @Override
    public void enterRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener)
        ((ExpressionListener) listener).enterUnaryNot(this);
    }

    @Override
    public void exitRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener)
        ((ExpressionListener) listener).exitUnaryNot(this);
    }

    @Override
    public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
      if (visitor instanceof ExpressionVisitor)
        return ((ExpressionVisitor<? extends T>) visitor).visitUnaryNot(this);
      else return visitor.visitChildren(this);
    }
  }

  public static class LikeContext extends ExprContext {
    public List<ExprContext> expr() {
      return getRuleContexts(ExprContext.class);
    }

    public ExprContext expr(int i) {
      return getRuleContext(ExprContext.class, i);
    }

    public TerminalNode LIKE() {
      return getToken(ExpressionParser.LIKE, 0);
    }

    public TerminalNode NOT_LIKE() {
      return getToken(ExpressionParser.NOT_LIKE, 0);
    }

    public LikeContext(ExprContext ctx) {
      copyFrom(ctx);
    }

    @Override
    public void enterRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener) ((ExpressionListener) listener).enterLike(this);
    }

    @Override
    public void exitRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener) ((ExpressionListener) listener).exitLike(this);
    }

    @Override
    public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
      if (visitor instanceof ExpressionVisitor)
        return ((ExpressionVisitor<? extends T>) visitor).visitLike(this);
      else return visitor.visitChildren(this);
    }
  }

  public static class ColumnContext extends ExprContext {
    public TerminalNode COLUMN_NAME() {
      return getToken(ExpressionParser.COLUMN_NAME, 0);
    }

    public ColumnContext(ExprContext ctx) {
      copyFrom(ctx);
    }

    @Override
    public void enterRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener) ((ExpressionListener) listener).enterColumn(this);
    }

    @Override
    public void exitRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener) ((ExpressionListener) listener).exitColumn(this);
    }

    @Override
    public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
      if (visitor instanceof ExpressionVisitor)
        return ((ExpressionVisitor<? extends T>) visitor).visitColumn(this);
      else return visitor.visitChildren(this);
    }
  }

  public static class MultDivModContext extends ExprContext {
    public Token op;

    public List<ExprContext> expr() {
      return getRuleContexts(ExprContext.class);
    }

    public ExprContext expr(int i) {
      return getRuleContext(ExprContext.class, i);
    }

    public TerminalNode MULTIPLY() {
      return getToken(ExpressionParser.MULTIPLY, 0);
    }

    public TerminalNode DIVIDE() {
      return getToken(ExpressionParser.DIVIDE, 0);
    }

    public TerminalNode MODULO() {
      return getToken(ExpressionParser.MODULO, 0);
    }

    public MultDivModContext(ExprContext ctx) {
      copyFrom(ctx);
    }

    @Override
    public void enterRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener)
        ((ExpressionListener) listener).enterMultDivMod(this);
    }

    @Override
    public void exitRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener)
        ((ExpressionListener) listener).exitMultDivMod(this);
    }

    @Override
    public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
      if (visitor instanceof ExpressionVisitor)
        return ((ExpressionVisitor<? extends T>) visitor).visitMultDivMod(this);
      else return visitor.visitChildren(this);
    }
  }

  public static class AndContext extends ExprContext {
    public Token op;

    public List<ExprContext> expr() {
      return getRuleContexts(ExprContext.class);
    }

    public ExprContext expr(int i) {
      return getRuleContext(ExprContext.class, i);
    }

    public TerminalNode AND() {
      return getToken(ExpressionParser.AND, 0);
    }

    public AndContext(ExprContext ctx) {
      copyFrom(ctx);
    }

    @Override
    public void enterRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener) ((ExpressionListener) listener).enterAnd(this);
    }

    @Override
    public void exitRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener) ((ExpressionListener) listener).exitAnd(this);
    }

    @Override
    public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
      if (visitor instanceof ExpressionVisitor)
        return ((ExpressionVisitor<? extends T>) visitor).visitAnd(this);
      else return visitor.visitChildren(this);
    }
  }

  public static class LiteralContext extends ExprContext {
    public ValueContext value() {
      return getRuleContext(ValueContext.class, 0);
    }

    public LiteralContext(ExprContext ctx) {
      copyFrom(ctx);
    }

    @Override
    public void enterRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener)
        ((ExpressionListener) listener).enterLiteral(this);
    }

    @Override
    public void exitRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener) ((ExpressionListener) listener).exitLiteral(this);
    }

    @Override
    public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
      if (visitor instanceof ExpressionVisitor)
        return ((ExpressionVisitor<? extends T>) visitor).visitLiteral(this);
      else return visitor.visitChildren(this);
    }
  }

  public static class CompareContext extends ExprContext {
    public Token op;

    public List<ExprContext> expr() {
      return getRuleContexts(ExprContext.class);
    }

    public ExprContext expr(int i) {
      return getRuleContext(ExprContext.class, i);
    }

    public TerminalNode EQUALS() {
      return getToken(ExpressionParser.EQUALS, 0);
    }

    public TerminalNode NOT_EQUALS() {
      return getToken(ExpressionParser.NOT_EQUALS, 0);
    }

    public TerminalNode LESS_THAN_OR_EQUAL() {
      return getToken(ExpressionParser.LESS_THAN_OR_EQUAL, 0);
    }

    public TerminalNode GREATER_THAN_OR_EQUAL() {
      return getToken(ExpressionParser.GREATER_THAN_OR_EQUAL, 0);
    }

    public TerminalNode LESS_THAN() {
      return getToken(ExpressionParser.LESS_THAN, 0);
    }

    public TerminalNode GREATER_THAN() {
      return getToken(ExpressionParser.GREATER_THAN, 0);
    }

    public CompareContext(ExprContext ctx) {
      copyFrom(ctx);
    }

    @Override
    public void enterRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener)
        ((ExpressionListener) listener).enterCompare(this);
    }

    @Override
    public void exitRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener) ((ExpressionListener) listener).exitCompare(this);
    }

    @Override
    public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
      if (visitor instanceof ExpressionVisitor)
        return ((ExpressionVisitor<? extends T>) visitor).visitCompare(this);
      else return visitor.visitChildren(this);
    }
  }

  public static class IsNullContext extends ExprContext {
    public ExprContext expr() {
      return getRuleContext(ExprContext.class, 0);
    }

    public TerminalNode IS_NULL() {
      return getToken(ExpressionParser.IS_NULL, 0);
    }

    public TerminalNode IS_EMPTY() {
      return getToken(ExpressionParser.IS_EMPTY, 0);
    }

    public TerminalNode IS_NOT_EMPTY() {
      return getToken(ExpressionParser.IS_NOT_EMPTY, 0);
    }

    public TerminalNode IS_NOT_NULL() {
      return getToken(ExpressionParser.IS_NOT_NULL, 0);
    }

    public IsNullContext(ExprContext ctx) {
      copyFrom(ctx);
    }

    @Override
    public void enterRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener) ((ExpressionListener) listener).enterIsNull(this);
    }

    @Override
    public void exitRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener) ((ExpressionListener) listener).exitIsNull(this);
    }

    @Override
    public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
      if (visitor instanceof ExpressionVisitor)
        return ((ExpressionVisitor<? extends T>) visitor).visitIsNull(this);
      else return visitor.visitChildren(this);
    }
  }

  public static class IfContext extends ExprContext {
    public TerminalNode IF() {
      return getToken(ExpressionParser.IF, 0);
    }

    public List<ExprContext> expr() {
      return getRuleContexts(ExprContext.class);
    }

    public ExprContext expr(int i) {
      return getRuleContext(ExprContext.class, i);
    }

    public TerminalNode THEN() {
      return getToken(ExpressionParser.THEN, 0);
    }

    public TerminalNode ELSE() {
      return getToken(ExpressionParser.ELSE, 0);
    }

    public TerminalNode END() {
      return getToken(ExpressionParser.END, 0);
    }

    public IfContext(ExprContext ctx) {
      copyFrom(ctx);
    }

    @Override
    public void enterRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener) ((ExpressionListener) listener).enterIf(this);
    }

    @Override
    public void exitRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener) ((ExpressionListener) listener).exitIf(this);
    }

    @Override
    public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
      if (visitor instanceof ExpressionVisitor)
        return ((ExpressionVisitor<? extends T>) visitor).visitIf(this);
      else return visitor.visitChildren(this);
    }
  }

  public static class PowerContext extends ExprContext {
    public Token op;

    public List<ExprContext> expr() {
      return getRuleContexts(ExprContext.class);
    }

    public ExprContext expr(int i) {
      return getRuleContext(ExprContext.class, i);
    }

    public TerminalNode POWER() {
      return getToken(ExpressionParser.POWER, 0);
    }

    public PowerContext(ExprContext ctx) {
      copyFrom(ctx);
    }

    @Override
    public void enterRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener) ((ExpressionListener) listener).enterPower(this);
    }

    @Override
    public void exitRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener) ((ExpressionListener) listener).exitPower(this);
    }

    @Override
    public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
      if (visitor instanceof ExpressionVisitor)
        return ((ExpressionVisitor<? extends T>) visitor).visitPower(this);
      else return visitor.visitChildren(this);
    }
  }

  public static class ParenContext extends ExprContext {
    public ExprContext expr() {
      return getRuleContext(ExprContext.class, 0);
    }

    public ParenContext(ExprContext ctx) {
      copyFrom(ctx);
    }

    @Override
    public void enterRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener) ((ExpressionListener) listener).enterParen(this);
    }

    @Override
    public void exitRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener) ((ExpressionListener) listener).exitParen(this);
    }

    @Override
    public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
      if (visitor instanceof ExpressionVisitor)
        return ((ExpressionVisitor<? extends T>) visitor).visitParen(this);
      else return visitor.visitChildren(this);
    }
  }

  public final ExprContext expr() throws RecognitionException {
    return expr(0);
  }

  private ExprContext expr(int _p) throws RecognitionException {
    ParserRuleContext _parentctx = _ctx;
    int _parentState = getState();
    ExprContext _localctx = new ExprContext(_ctx, _parentState);
    ExprContext _prevctx = _localctx;
    int _startState = 2;
    enterRecursionRule(_localctx, 2, RULE_expr, _p);
    int _la;
    try {
      int _alt;
      enterOuterAlt(_localctx, 1);
      {
        setState(42);
        _errHandler.sync(this);
        switch (_input.LA(1)) {
          case UNARY_NOT:
            {
              _localctx = new UnaryNotContext(_localctx);
              _ctx = _localctx;
              _prevctx = _localctx;

              setState(10);
              match(UNARY_NOT);
              setState(11);
              expr(9);
            }
            break;
          case IF:
            {
              _localctx = new IfContext(_localctx);
              _ctx = _localctx;
              _prevctx = _localctx;
              setState(12);
              match(IF);
              setState(13);
              expr(0);
              setState(14);
              match(THEN);
              setState(15);
              expr(0);
              setState(16);
              match(ELSE);
              setState(17);
              expr(0);
              setState(19);
              _errHandler.sync(this);
              switch (getInterpreter().adaptivePredict(_input, 0, _ctx)) {
                case 1:
                  {
                    setState(18);
                    match(END);
                  }
                  break;
              }
            }
            break;
          case IDENTIFIER:
            {
              _localctx = new FunctionContext(_localctx);
              _ctx = _localctx;
              _prevctx = _localctx;
              setState(21);
              match(IDENTIFIER);
              setState(22);
              match(T__0);
              setState(31);
              _errHandler.sync(this);
              _la = _input.LA(1);
              if ((((_la) & ~0x3f) == 0
                  && ((1L << _la)
                          & ((1L << T__0)
                              | (1L << T__3)
                              | (1L << MINUS)
                              | (1L << NULL)
                              | (1L << NOTHING)
                              | (1L << TRUE)
                              | (1L << FALSE)
                              | (1L << IF)
                              | (1L << UNARY_NOT)
                              | (1L << IDENTIFIER)
                              | (1L << EXCEL_STRING)
                              | (1L << PYTHON_STRING)
                              | (1L << NUMBER)
                              | (1L << COLUMN_NAME)))
                      != 0)) {
                {
                  setState(23);
                  expr(0);
                  setState(28);
                  _errHandler.sync(this);
                  _la = _input.LA(1);
                  while (_la == T__1) {
                    {
                      {
                        setState(24);
                        match(T__1);
                        setState(25);
                        expr(0);
                      }
                    }
                    setState(30);
                    _errHandler.sync(this);
                    _la = _input.LA(1);
                  }
                }
              }

              setState(33);
              match(T__2);
            }
            break;
          case T__0:
            {
              _localctx = new ParenContext(_localctx);
              _ctx = _localctx;
              _prevctx = _localctx;
              setState(34);
              match(T__0);
              setState(35);
              expr(0);
              setState(36);
              match(T__2);
            }
            break;
          case MINUS:
            {
              _localctx = new UnaryMinusContext(_localctx);
              _ctx = _localctx;
              _prevctx = _localctx;
              setState(38);
              match(MINUS);
              setState(39);
              expr(3);
            }
            break;
          case COLUMN_NAME:
            {
              _localctx = new ColumnContext(_localctx);
              _ctx = _localctx;
              _prevctx = _localctx;
              setState(40);
              match(COLUMN_NAME);
            }
            break;
          case T__3:
          case NULL:
          case NOTHING:
          case TRUE:
          case FALSE:
          case EXCEL_STRING:
          case PYTHON_STRING:
          case NUMBER:
            {
              _localctx = new LiteralContext(_localctx);
              _ctx = _localctx;
              _prevctx = _localctx;
              setState(41);
              value();
            }
            break;
          default:
            throw new NoViableAltException(this);
        }
        _ctx.stop = _input.LT(-1);
        setState(88);
        _errHandler.sync(this);
        _alt = getInterpreter().adaptivePredict(_input, 6, _ctx);
        while (_alt != 2 && _alt != org.antlr.v4.runtime.atn.ATN.INVALID_ALT_NUMBER) {
          if (_alt == 1) {
            if (_parseListeners != null) triggerExitRuleEvent();
            _prevctx = _localctx;
            {
              setState(86);
              _errHandler.sync(this);
              switch (getInterpreter().adaptivePredict(_input, 5, _ctx)) {
                case 1:
                  {
                    _localctx = new PowerContext(new ExprContext(_parentctx, _parentState));
                    pushNewRecursionContext(_localctx, _startState, RULE_expr);
                    setState(44);
                    if (!(precpred(_ctx, 17)))
                      throw new FailedPredicateException(this, "precpred(_ctx, 17)");
                    setState(45);
                    ((PowerContext) _localctx).op = match(POWER);
                    setState(46);
                    expr(18);
                  }
                  break;
                case 2:
                  {
                    _localctx = new MultDivModContext(new ExprContext(_parentctx, _parentState));
                    pushNewRecursionContext(_localctx, _startState, RULE_expr);
                    setState(47);
                    if (!(precpred(_ctx, 16)))
                      throw new FailedPredicateException(this, "precpred(_ctx, 16)");
                    setState(48);
                    ((MultDivModContext) _localctx).op = _input.LT(1);
                    _la = _input.LA(1);
                    if (!((((_la) & ~0x3f) == 0
                        && ((1L << _la) & ((1L << MULTIPLY) | (1L << DIVIDE) | (1L << MODULO)))
                            != 0))) {
                      ((MultDivModContext) _localctx).op = (Token) _errHandler.recoverInline(this);
                    } else {
                      if (_input.LA(1) == Token.EOF) matchedEOF = true;
                      _errHandler.reportMatch(this);
                      consume();
                    }
                    setState(49);
                    expr(17);
                  }
                  break;
                case 3:
                  {
                    _localctx = new AddSubContext(new ExprContext(_parentctx, _parentState));
                    pushNewRecursionContext(_localctx, _startState, RULE_expr);
                    setState(50);
                    if (!(precpred(_ctx, 15)))
                      throw new FailedPredicateException(this, "precpred(_ctx, 15)");
                    setState(51);
                    ((AddSubContext) _localctx).op = _input.LT(1);
                    _la = _input.LA(1);
                    if (!(_la == ADD || _la == MINUS)) {
                      ((AddSubContext) _localctx).op = (Token) _errHandler.recoverInline(this);
                    } else {
                      if (_input.LA(1) == Token.EOF) matchedEOF = true;
                      _errHandler.reportMatch(this);
                      consume();
                    }
                    setState(52);
                    expr(16);
                  }
                  break;
                case 4:
                  {
                    _localctx = new CompareContext(new ExprContext(_parentctx, _parentState));
                    pushNewRecursionContext(_localctx, _startState, RULE_expr);
                    setState(53);
                    if (!(precpred(_ctx, 14)))
                      throw new FailedPredicateException(this, "precpred(_ctx, 14)");
                    setState(54);
                    ((CompareContext) _localctx).op = _input.LT(1);
                    _la = _input.LA(1);
                    if (!((((_la) & ~0x3f) == 0
                        && ((1L << _la)
                                & ((1L << EQUALS)
                                    | (1L << NOT_EQUALS)
                                    | (1L << LESS_THAN_OR_EQUAL)
                                    | (1L << GREATER_THAN_OR_EQUAL)
                                    | (1L << LESS_THAN)
                                    | (1L << GREATER_THAN)))
                            != 0))) {
                      ((CompareContext) _localctx).op = (Token) _errHandler.recoverInline(this);
                    } else {
                      if (_input.LA(1) == Token.EOF) matchedEOF = true;
                      _errHandler.reportMatch(this);
                      consume();
                    }
                    setState(55);
                    expr(15);
                  }
                  break;
                case 5:
                  {
                    _localctx = new LikeContext(new ExprContext(_parentctx, _parentState));
                    pushNewRecursionContext(_localctx, _startState, RULE_expr);
                    setState(56);
                    if (!(precpred(_ctx, 12)))
                      throw new FailedPredicateException(this, "precpred(_ctx, 12)");
                    setState(57);
                    _la = _input.LA(1);
                    if (!(_la == LIKE || _la == NOT_LIKE)) {
                      _errHandler.recoverInline(this);
                    } else {
                      if (_input.LA(1) == Token.EOF) matchedEOF = true;
                      _errHandler.reportMatch(this);
                      consume();
                    }
                    setState(58);
                    expr(13);
                  }
                  break;
                case 6:
                  {
                    _localctx = new BetweenContext(new ExprContext(_parentctx, _parentState));
                    pushNewRecursionContext(_localctx, _startState, RULE_expr);
                    setState(59);
                    if (!(precpred(_ctx, 10)))
                      throw new FailedPredicateException(this, "precpred(_ctx, 10)");
                    setState(60);
                    _la = _input.LA(1);
                    if (!(_la == BETWEEN || _la == NOT_BETWEEN)) {
                      _errHandler.recoverInline(this);
                    } else {
                      if (_input.LA(1) == Token.EOF) matchedEOF = true;
                      _errHandler.reportMatch(this);
                      consume();
                    }
                    setState(61);
                    expr(0);
                    setState(62);
                    match(AND);
                    setState(63);
                    expr(11);
                  }
                  break;
                case 7:
                  {
                    _localctx = new AndContext(new ExprContext(_parentctx, _parentState));
                    pushNewRecursionContext(_localctx, _startState, RULE_expr);
                    setState(65);
                    if (!(precpred(_ctx, 8)))
                      throw new FailedPredicateException(this, "precpred(_ctx, 8)");
                    setState(66);
                    ((AndContext) _localctx).op = match(AND);
                    setState(67);
                    expr(9);
                  }
                  break;
                case 8:
                  {
                    _localctx = new OrContext(new ExprContext(_parentctx, _parentState));
                    pushNewRecursionContext(_localctx, _startState, RULE_expr);
                    setState(68);
                    if (!(precpred(_ctx, 7)))
                      throw new FailedPredicateException(this, "precpred(_ctx, 7)");
                    setState(69);
                    ((OrContext) _localctx).op = match(OR);
                    setState(70);
                    expr(8);
                  }
                  break;
                case 9:
                  {
                    _localctx = new IsNullContext(new ExprContext(_parentctx, _parentState));
                    pushNewRecursionContext(_localctx, _startState, RULE_expr);
                    setState(71);
                    if (!(precpred(_ctx, 13)))
                      throw new FailedPredicateException(this, "precpred(_ctx, 13)");
                    setState(72);
                    _la = _input.LA(1);
                    if (!((((_la) & ~0x3f) == 0
                        && ((1L << _la)
                                & ((1L << IS_NULL)
                                    | (1L << IS_NOT_NULL)
                                    | (1L << IS_EMPTY)
                                    | (1L << IS_NOT_EMPTY)))
                            != 0))) {
                      _errHandler.recoverInline(this);
                    } else {
                      if (_input.LA(1) == Token.EOF) matchedEOF = true;
                      _errHandler.reportMatch(this);
                      consume();
                    }
                  }
                  break;
                case 10:
                  {
                    _localctx = new InContext(new ExprContext(_parentctx, _parentState));
                    pushNewRecursionContext(_localctx, _startState, RULE_expr);
                    setState(73);
                    if (!(precpred(_ctx, 11)))
                      throw new FailedPredicateException(this, "precpred(_ctx, 11)");
                    setState(74);
                    match(IN);
                    setState(75);
                    match(T__0);
                    setState(76);
                    expr(0);
                    setState(81);
                    _errHandler.sync(this);
                    _la = _input.LA(1);
                    while (_la == T__1) {
                      {
                        {
                          setState(77);
                          match(T__1);
                          setState(78);
                          expr(0);
                        }
                      }
                      setState(83);
                      _errHandler.sync(this);
                      _la = _input.LA(1);
                    }
                    setState(84);
                    match(T__2);
                  }
                  break;
              }
            }
          }
          setState(90);
          _errHandler.sync(this);
          _alt = getInterpreter().adaptivePredict(_input, 6, _ctx);
        }
      }
    } catch (RecognitionException re) {
      _localctx.exception = re;
      _errHandler.reportError(this, re);
      _errHandler.recover(this, re);
    } finally {
      unrollRecursionContexts(_parentctx);
    }
    return _localctx;
  }

  public static class ValueContext extends ParserRuleContext {
    public ValueContext(ParserRuleContext parent, int invokingState) {
      super(parent, invokingState);
    }

    @Override
    public int getRuleIndex() {
      return RULE_value;
    }

    public ValueContext() {}

    public void copyFrom(ValueContext ctx) {
      super.copyFrom(ctx);
    }
  }

  public static class DateContext extends ValueContext {
    public Token text;

    public TerminalNode DATE() {
      return getToken(ExpressionParser.DATE, 0);
    }

    public DateContext(ValueContext ctx) {
      copyFrom(ctx);
    }

    @Override
    public void enterRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener) ((ExpressionListener) listener).enterDate(this);
    }

    @Override
    public void exitRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener) ((ExpressionListener) listener).exitDate(this);
    }

    @Override
    public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
      if (visitor instanceof ExpressionVisitor)
        return ((ExpressionVisitor<? extends T>) visitor).visitDate(this);
      else return visitor.visitChildren(this);
    }
  }

  public static class NumberContext extends ValueContext {
    public TerminalNode NUMBER() {
      return getToken(ExpressionParser.NUMBER, 0);
    }

    public NumberContext(ValueContext ctx) {
      copyFrom(ctx);
    }

    @Override
    public void enterRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener) ((ExpressionListener) listener).enterNumber(this);
    }

    @Override
    public void exitRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener) ((ExpressionListener) listener).exitNumber(this);
    }

    @Override
    public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
      if (visitor instanceof ExpressionVisitor)
        return ((ExpressionVisitor<? extends T>) visitor).visitNumber(this);
      else return visitor.visitChildren(this);
    }
  }

  public static class DatetimeContext extends ValueContext {
    public Token text;

    public TerminalNode DATE_TIME() {
      return getToken(ExpressionParser.DATE_TIME, 0);
    }

    public DatetimeContext(ValueContext ctx) {
      copyFrom(ctx);
    }

    @Override
    public void enterRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener)
        ((ExpressionListener) listener).enterDatetime(this);
    }

    @Override
    public void exitRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener)
        ((ExpressionListener) listener).exitDatetime(this);
    }

    @Override
    public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
      if (visitor instanceof ExpressionVisitor)
        return ((ExpressionVisitor<? extends T>) visitor).visitDatetime(this);
      else return visitor.visitChildren(this);
    }
  }

  public static class PythonStringContext extends ValueContext {
    public TerminalNode PYTHON_STRING() {
      return getToken(ExpressionParser.PYTHON_STRING, 0);
    }

    public PythonStringContext(ValueContext ctx) {
      copyFrom(ctx);
    }

    @Override
    public void enterRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener)
        ((ExpressionListener) listener).enterPythonString(this);
    }

    @Override
    public void exitRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener)
        ((ExpressionListener) listener).exitPythonString(this);
    }

    @Override
    public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
      if (visitor instanceof ExpressionVisitor)
        return ((ExpressionVisitor<? extends T>) visitor).visitPythonString(this);
      else return visitor.visitChildren(this);
    }
  }

  public static class BooleanContext extends ValueContext {
    public TerminalNode TRUE() {
      return getToken(ExpressionParser.TRUE, 0);
    }

    public TerminalNode FALSE() {
      return getToken(ExpressionParser.FALSE, 0);
    }

    public BooleanContext(ValueContext ctx) {
      copyFrom(ctx);
    }

    @Override
    public void enterRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener)
        ((ExpressionListener) listener).enterBoolean(this);
    }

    @Override
    public void exitRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener) ((ExpressionListener) listener).exitBoolean(this);
    }

    @Override
    public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
      if (visitor instanceof ExpressionVisitor)
        return ((ExpressionVisitor<? extends T>) visitor).visitBoolean(this);
      else return visitor.visitChildren(this);
    }
  }

  public static class ExcelStringContext extends ValueContext {
    public TerminalNode EXCEL_STRING() {
      return getToken(ExpressionParser.EXCEL_STRING, 0);
    }

    public ExcelStringContext(ValueContext ctx) {
      copyFrom(ctx);
    }

    @Override
    public void enterRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener)
        ((ExpressionListener) listener).enterExcelString(this);
    }

    @Override
    public void exitRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener)
        ((ExpressionListener) listener).exitExcelString(this);
    }

    @Override
    public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
      if (visitor instanceof ExpressionVisitor)
        return ((ExpressionVisitor<? extends T>) visitor).visitExcelString(this);
      else return visitor.visitChildren(this);
    }
  }

  public static class TimeContext extends ValueContext {
    public Token text;

    public TerminalNode TIME() {
      return getToken(ExpressionParser.TIME, 0);
    }

    public TimeContext(ValueContext ctx) {
      copyFrom(ctx);
    }

    @Override
    public void enterRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener) ((ExpressionListener) listener).enterTime(this);
    }

    @Override
    public void exitRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener) ((ExpressionListener) listener).exitTime(this);
    }

    @Override
    public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
      if (visitor instanceof ExpressionVisitor)
        return ((ExpressionVisitor<? extends T>) visitor).visitTime(this);
      else return visitor.visitChildren(this);
    }
  }

  public static class NullOrNothingContext extends ValueContext {
    public TerminalNode NULL() {
      return getToken(ExpressionParser.NULL, 0);
    }

    public TerminalNode NOTHING() {
      return getToken(ExpressionParser.NOTHING, 0);
    }

    public NullOrNothingContext(ValueContext ctx) {
      copyFrom(ctx);
    }

    @Override
    public void enterRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener)
        ((ExpressionListener) listener).enterNullOrNothing(this);
    }

    @Override
    public void exitRule(ParseTreeListener listener) {
      if (listener instanceof ExpressionListener)
        ((ExpressionListener) listener).exitNullOrNothing(this);
    }

    @Override
    public <T> T accept(ParseTreeVisitor<? extends T> visitor) {
      if (visitor instanceof ExpressionVisitor)
        return ((ExpressionVisitor<? extends T>) visitor).visitNullOrNothing(this);
      else return visitor.visitChildren(this);
    }
  }

  public final ValueContext value() throws RecognitionException {
    ValueContext _localctx = new ValueContext(_ctx, getState());
    enterRule(_localctx, 4, RULE_value);
    int _la;
    try {
      setState(105);
      _errHandler.sync(this);
      switch (getInterpreter().adaptivePredict(_input, 7, _ctx)) {
        case 1:
          _localctx = new NullOrNothingContext(_localctx);
          enterOuterAlt(_localctx, 1);
          {
            setState(91);
            _la = _input.LA(1);
            if (!(_la == NULL || _la == NOTHING)) {
              _errHandler.recoverInline(this);
            } else {
              if (_input.LA(1) == Token.EOF) matchedEOF = true;
              _errHandler.reportMatch(this);
              consume();
            }
          }
          break;
        case 2:
          _localctx = new BooleanContext(_localctx);
          enterOuterAlt(_localctx, 2);
          {
            setState(92);
            _la = _input.LA(1);
            if (!(_la == TRUE || _la == FALSE)) {
              _errHandler.recoverInline(this);
            } else {
              if (_input.LA(1) == Token.EOF) matchedEOF = true;
              _errHandler.reportMatch(this);
              consume();
            }
          }
          break;
        case 3:
          _localctx = new DateContext(_localctx);
          enterOuterAlt(_localctx, 3);
          {
            setState(93);
            match(T__3);
            setState(94);
            ((DateContext) _localctx).text = match(DATE);
            setState(95);
            match(T__3);
          }
          break;
        case 4:
          _localctx = new TimeContext(_localctx);
          enterOuterAlt(_localctx, 4);
          {
            setState(96);
            match(T__3);
            setState(97);
            ((TimeContext) _localctx).text = match(TIME);
            setState(98);
            match(T__3);
          }
          break;
        case 5:
          _localctx = new DatetimeContext(_localctx);
          enterOuterAlt(_localctx, 5);
          {
            setState(99);
            match(T__3);
            setState(100);
            ((DatetimeContext) _localctx).text = match(DATE_TIME);
            setState(101);
            match(T__3);
          }
          break;
        case 6:
          _localctx = new NumberContext(_localctx);
          enterOuterAlt(_localctx, 6);
          {
            setState(102);
            match(NUMBER);
          }
          break;
        case 7:
          _localctx = new ExcelStringContext(_localctx);
          enterOuterAlt(_localctx, 7);
          {
            setState(103);
            match(EXCEL_STRING);
          }
          break;
        case 8:
          _localctx = new PythonStringContext(_localctx);
          enterOuterAlt(_localctx, 8);
          {
            setState(104);
            match(PYTHON_STRING);
          }
          break;
      }
    } catch (RecognitionException re) {
      _localctx.exception = re;
      _errHandler.reportError(this, re);
      _errHandler.recover(this, re);
    } finally {
      exitRule();
    }
    return _localctx;
  }

  public boolean sempred(RuleContext _localctx, int ruleIndex, int predIndex) {
    switch (ruleIndex) {
      case 1:
        return expr_sempred((ExprContext) _localctx, predIndex);
    }
    return true;
  }

  private boolean expr_sempred(ExprContext _localctx, int predIndex) {
    switch (predIndex) {
      case 0:
        return precpred(_ctx, 17);
      case 1:
        return precpred(_ctx, 16);
      case 2:
        return precpred(_ctx, 15);
      case 3:
        return precpred(_ctx, 14);
      case 4:
        return precpred(_ctx, 12);
      case 5:
        return precpred(_ctx, 10);
      case 6:
        return precpred(_ctx, 8);
      case 7:
        return precpred(_ctx, 7);
      case 8:
        return precpred(_ctx, 13);
      case 9:
        return precpred(_ctx, 11);
    }
    return true;
  }

  public static final String _serializedATN =
      "\u0004\u0001-l\u0002\u0000\u0007\u0000\u0002\u0001\u0007\u0001\u0002\u0002"
          + "\u0007\u0002\u0001\u0000\u0001\u0000\u0001\u0000\u0001\u0001\u0001\u0001"
          + "\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001"
          + "\u0001\u0001\u0001\u0001\u0003\u0001\u0014\b\u0001\u0001\u0001\u0001\u0001"
          + "\u0001\u0001\u0001\u0001\u0001\u0001\u0005\u0001\u001b\b\u0001\n\u0001"
          + "\f\u0001\u001e\t\u0001\u0003\u0001 \b\u0001\u0001\u0001\u0001\u0001\u0001"
          + "\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001"
          + "\u0001\u0003\u0001+\b\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001"
          + "\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001"
          + "\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001"
          + "\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001"
          + "\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001"
          + "\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001\u0001"
          + "\u0001\u0001\u0001\u0005\u0001P\b\u0001\n\u0001\f\u0001S\t\u0001\u0001"
          + "\u0001\u0001\u0001\u0005\u0001W\b\u0001\n\u0001\f\u0001Z\t\u0001\u0001"
          + "\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0001"
          + "\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0001\u0002\u0001"
          + "\u0002\u0001\u0002\u0003\u0002j\b\u0002\u0001\u0002\u0000\u0001\u0002"
          + "\u0003\u0000\u0002\u0004\u0000\b\u0001\u0000\u0006\b\u0001\u0000\t\n\u0001"
          + "\u0000\u000b\u0010\u0001\u0000\u001a\u001b\u0001\u0000\u001d\u001e\u0001"
          + "\u0000\u0016\u0019\u0001\u0000\u0014\u0015\u0001\u0000\u001f \u0083\u0000"
          + "\u0006\u0001\u0000\u0000\u0000\u0002*\u0001\u0000\u0000\u0000\u0004i\u0001"
          + "\u0000\u0000\u0000\u0006\u0007\u0003\u0002\u0001\u0000\u0007\b\u0005\u0000"
          + "\u0000\u0001\b\u0001\u0001\u0000\u0000\u0000\t\n\u0006\u0001\uffff\uffff"
          + "\u0000\n\u000b\u0005$\u0000\u0000\u000b+\u0003\u0002\u0001\t\f\r\u0005"
          + "!\u0000\u0000\r\u000e\u0003\u0002\u0001\u0000\u000e\u000f\u0005\"\u0000"
          + "\u0000\u000f\u0010\u0003\u0002\u0001\u0000\u0010\u0011\u0005#\u0000\u0000"
          + "\u0011\u0013\u0003\u0002\u0001\u0000\u0012\u0014\u0005%\u0000\u0000\u0013"
          + "\u0012\u0001\u0000\u0000\u0000\u0013\u0014\u0001\u0000\u0000\u0000\u0014"
          + "+\u0001\u0000\u0000\u0000\u0015\u0016\u0005&\u0000\u0000\u0016\u001f\u0005"
          + "\u0001\u0000\u0000\u0017\u001c\u0003\u0002\u0001\u0000\u0018\u0019\u0005"
          + "\u0002\u0000\u0000\u0019\u001b\u0003\u0002\u0001\u0000\u001a\u0018\u0001"
          + "\u0000\u0000\u0000\u001b\u001e\u0001\u0000\u0000\u0000\u001c\u001a\u0001"
          + "\u0000\u0000\u0000\u001c\u001d\u0001\u0000\u0000\u0000\u001d \u0001\u0000"
          + "\u0000\u0000\u001e\u001c\u0001\u0000\u0000\u0000\u001f\u0017\u0001\u0000"
          + "\u0000\u0000\u001f \u0001\u0000\u0000\u0000 !\u0001\u0000\u0000\u0000"
          + "!+\u0005\u0003\u0000\u0000\"#\u0005\u0001\u0000\u0000#$\u0003\u0002\u0001"
          + "\u0000$%\u0005\u0003\u0000\u0000%+\u0001\u0000\u0000\u0000&\'\u0005\n"
          + "\u0000\u0000\'+\u0003\u0002\u0001\u0003(+\u0005-\u0000\u0000)+\u0003\u0004"
          + "\u0002\u0000*\t\u0001\u0000\u0000\u0000*\f\u0001\u0000\u0000\u0000*\u0015"
          + "\u0001\u0000\u0000\u0000*\"\u0001\u0000\u0000\u0000*&\u0001\u0000\u0000"
          + "\u0000*(\u0001\u0000\u0000\u0000*)\u0001\u0000\u0000\u0000+X\u0001\u0000"
          + "\u0000\u0000,-\n\u0011\u0000\u0000-.\u0005\u0005\u0000\u0000.W\u0003\u0002"
          + "\u0001\u0012/0\n\u0010\u0000\u000001\u0007\u0000\u0000\u00001W\u0003\u0002"
          + "\u0001\u001123\n\u000f\u0000\u000034\u0007\u0001\u0000\u00004W\u0003\u0002"
          + "\u0001\u001056\n\u000e\u0000\u000067\u0007\u0002\u0000\u00007W\u0003\u0002"
          + "\u0001\u000f89\n\f\u0000\u00009:\u0007\u0003\u0000\u0000:W\u0003\u0002"
          + "\u0001\r;<\n\n\u0000\u0000<=\u0007\u0004\u0000\u0000=>\u0003\u0002\u0001"
          + "\u0000>?\u0005\u0012\u0000\u0000?@\u0003\u0002\u0001\u000b@W\u0001\u0000"
          + "\u0000\u0000AB\n\b\u0000\u0000BC\u0005\u0012\u0000\u0000CW\u0003\u0002"
          + "\u0001\tDE\n\u0007\u0000\u0000EF\u0005\u0013\u0000\u0000FW\u0003\u0002"
          + "\u0001\bGH\n\r\u0000\u0000HW\u0007\u0005\u0000\u0000IJ\n\u000b\u0000\u0000"
          + "JK\u0005\u001c\u0000\u0000KL\u0005\u0001\u0000\u0000LQ\u0003\u0002\u0001"
          + "\u0000MN\u0005\u0002\u0000\u0000NP\u0003\u0002\u0001\u0000OM\u0001\u0000"
          + "\u0000\u0000PS\u0001\u0000\u0000\u0000QO\u0001\u0000\u0000\u0000QR\u0001"
          + "\u0000\u0000\u0000RT\u0001\u0000\u0000\u0000SQ\u0001\u0000\u0000\u0000"
          + "TU\u0005\u0003\u0000\u0000UW\u0001\u0000\u0000\u0000V,\u0001\u0000\u0000"
          + "\u0000V/\u0001\u0000\u0000\u0000V2\u0001\u0000\u0000\u0000V5\u0001\u0000"
          + "\u0000\u0000V8\u0001\u0000\u0000\u0000V;\u0001\u0000\u0000\u0000VA\u0001"
          + "\u0000\u0000\u0000VD\u0001\u0000\u0000\u0000VG\u0001\u0000\u0000\u0000"
          + "VI\u0001\u0000\u0000\u0000WZ\u0001\u0000\u0000\u0000XV\u0001\u0000\u0000"
          + "\u0000XY\u0001\u0000\u0000\u0000Y\u0003\u0001\u0000\u0000\u0000ZX\u0001"
          + "\u0000\u0000\u0000[j\u0007\u0006\u0000\u0000\\j\u0007\u0007\u0000\u0000"
          + "]^\u0005\u0004\u0000\u0000^_\u0005)\u0000\u0000_j\u0005\u0004\u0000\u0000"
          + "`a\u0005\u0004\u0000\u0000ab\u0005*\u0000\u0000bj\u0005\u0004\u0000\u0000"
          + "cd\u0005\u0004\u0000\u0000de\u0005+\u0000\u0000ej\u0005\u0004\u0000\u0000"
          + "fj\u0005,\u0000\u0000gj\u0005\'\u0000\u0000hj\u0005(\u0000\u0000i[\u0001"
          + "\u0000\u0000\u0000i\\\u0001\u0000\u0000\u0000i]\u0001\u0000\u0000\u0000"
          + "i`\u0001\u0000\u0000\u0000ic\u0001\u0000\u0000\u0000if\u0001\u0000\u0000"
          + "\u0000ig\u0001\u0000\u0000\u0000ih\u0001\u0000\u0000\u0000j\u0005\u0001"
          + "\u0000\u0000\u0000\b\u0013\u001c\u001f*QVXi";
  public static final ATN _ATN = new ATNDeserializer().deserialize(_serializedATN.toCharArray());

  static {
    _decisionToDFA = new DFA[_ATN.getNumberOfDecisions()];
    for (int i = 0; i < _ATN.getNumberOfDecisions(); i++) {
      _decisionToDFA[i] = new DFA(_ATN.getDecisionState(i), i);
    }
  }
}
