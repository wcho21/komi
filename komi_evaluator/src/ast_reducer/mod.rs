mod assignment_infix;
mod branch;
mod call;
mod combinator_infix;
mod comparison_infix;
mod expressions;
mod leaf;
mod prefix;
mod util;

use crate::ValRes;
use crate::environment::Environment as Env;
use assignment_infix as assign_infix;
use combinator_infix as comb_infix;
use comparison_infix as comp_infix;
use komi_syntax::ast::{Ast, AstKind};
use komi_syntax::value::Stdout;

type Args = Vec<Box<Ast>>;
type Params = Vec<String>;
type Exprs = Vec<Box<Ast>>;

pub fn reduce_ast(ast: &Box<Ast>, env: &mut Env, stdouts: &mut Stdout) -> ValRes {
    // Design principle: once you read something, pass it as an argument.
    // This avoids unnecessary repeated reading in subfunctions.
    // Moreover, if you delay determining the kind of what you read, the decision is only postponed to subfunctions.
    // You'll have to handle exceptional cases that could have been avoided.

    let loc = ast.location;
    match &ast.kind {
        AstKind::Program { expressions: e } => expressions::reduce(&e, &loc, env, stdouts),
        AstKind::Identifier(id) => leaf::evaluate_identifier(id, &loc, env),
        AstKind::Number(n) => leaf::evaluate_num(*n, &loc),
        AstKind::Bool(b) => leaf::evaluate_bool(*b, &loc),
        AstKind::Str(s) => leaf::evaluate_str(s, &loc, env),
        AstKind::Closure { parameters: p, body: b } => leaf::evaluate_closure(p, b, &loc, env),
        AstKind::PrefixPlus { operand: op } => prefix::reduce_plus(&op, &loc, env, stdouts),
        AstKind::PrefixMinus { operand: op } => prefix::reduce_minus(&op, &loc, env, stdouts),
        AstKind::PrefixBang { operand: op } => prefix::reduce_bang(&op, &loc, env, stdouts),
        AstKind::InfixPlus { left: l, right: r } => comb_infix::reduce_plus(&l, &r, &loc, env, stdouts),
        AstKind::InfixMinus { left: l, right: r } => comb_infix::reduce_minus(&l, &r, &loc, env, stdouts),
        AstKind::InfixAsterisk { left: l, right: r } => comb_infix::reduce_asterisk(&l, &r, &loc, env, stdouts),
        AstKind::InfixSlash { left: l, right: r } => comb_infix::reduce_slash(&l, &r, &loc, env, stdouts),
        AstKind::InfixPercent { left: l, right: r } => comb_infix::reduce_percent(&l, &r, &loc, env, stdouts),
        AstKind::InfixConjunct { left: l, right: r } => comb_infix::reduce_conjunct(&l, &r, &loc, env, stdouts),
        AstKind::InfixDisjunct { left: l, right: r } => comb_infix::reduce_disjunct(&l, &r, &loc, env, stdouts),
        AstKind::InfixEquals { left: l, right: r } => assign_infix::reduce_equals(&l, &r, &loc, env, stdouts),
        AstKind::InfixPlusEquals { left: l, right: r } => assign_infix::reduce_plus_equals(&l, &r, &loc, env, stdouts),
        AstKind::InfixMinusEquals { left: l, right: r } => {
            assign_infix::reduce_minus_equals(&l, &r, &loc, env, stdouts)
        }
        AstKind::InfixAsteriskEquals { left: l, right: r } => {
            assign_infix::reduce_asterisk_equals(&l, &r, &loc, env, stdouts)
        }
        AstKind::InfixSlashEquals { left: l, right: r } => {
            assign_infix::reduce_slash_equals(&l, &r, &loc, env, stdouts)
        }
        AstKind::InfixPercentEquals { left: l, right: r } => {
            assign_infix::reduce_percent_equals(&l, &r, &loc, env, stdouts)
        }
        AstKind::InfixDoubleEquals { left: l, right: r } => {
            comp_infix::reduce_double_equals(&l, &r, &loc, env, stdouts)
        }
        AstKind::InfixBangEquals { left: l, right: r } => comp_infix::reduce_bang_equals(&l, &r, &loc, env, stdouts),
        AstKind::InfixLBracket { left: l, right: r } => comp_infix::reduce_lbracket(&l, &r, &loc, env, stdouts),
        AstKind::InfixRBracket { left: l, right: r } => comp_infix::reduce_rbracket(&l, &r, &loc, env, stdouts),
        AstKind::Call { target: t, arguments: args } => call::evaluate(t, args, &loc, env, stdouts),
        AstKind::Branch { predicate: p, consequence: c, alternative: a } => {
            branch::evaluate(p, c, a, &loc, env, stdouts)
        }
        _ => todo!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fixtures::*;
    use komi_syntax::ast::AstKind;
    use komi_syntax::error::{EvalError, EvalErrorKind};
    use komi_syntax::value::{Value, ValueKind};
    use komi_syntax::{mkast, mkval};
    use komi_util::location::Range;
    use komi_util::str_segment::{StrSegment, StrSegmentKind};
    use komi_util::{mkstrseg, str_loc};
    use rstest::rstest;

    /// Asserts a given AST to be evaluated into the expected value.
    /// Helps write a test declaratively.
    macro_rules! assert_eval {
        ($ast:expr, $expected:expr $(,)?) => {{
            let mut env = Env::new();
            let mut stdouts: Stdout = vec![];
            assert_eq!(
                reduce_ast($ast, &mut env, &mut stdouts),
                Ok($expected),
                "received a value (left) evaluated from the ast, but expected the different value (right)",
            );
        }};
        ($ast:expr, $env:expr, $expected:expr $(,)?) => {{
            let mut stdouts: Stdout = vec![];
            assert_eq!(
                reduce_ast($ast, $env, &mut stdouts),
                Ok($expected),
                "received a value (left) evaluated from the ast, but expected the different value (right)",
            );
        }};
    }

    /// Asserts evaluating a given AST will fail.
    /// Helps write a test declaratively.
    macro_rules! assert_eval_fail {
        ($ast:expr, $expected:expr $(,)?) => {{
            let mut env = Env::new();
            let mut stdouts: Stdout = vec![];
            assert_eq!(
                reduce_ast($ast, &mut env, &mut stdouts),
                Err($expected),
                "received a result (left), but expected an error (right)",
            );
        }};
        ($ast:expr, $env:expr, $expected:expr $(,)?) => {{
            let mut stdouts: Stdout = vec![];
            assert_eq!(
                reduce_ast($ast, $env, &mut stdouts),
                Err($expected),
                "received a result (left), but expected an error (right)",
            );
        }};
    }

    /// Makes a `EvalError`.
    /// The first argument is the error kind `EvalErrorKind`.
    /// The second argument is the error location `Range`.
    macro_rules! mkerr {
        ($kind:ident, $range:expr) => {
            EvalError::new(EvalErrorKind::$kind, $range)
        };
    }

    #[test]
    fn empty() {
        // Represents ``.
        assert_eval_fail!(
            &mkast!(prog loc 0, 0, 0, 0, vec![]),
            mkerr!(NoExpressions, str_loc!("", "")),
        );
    }

    mod call {
        use super::*;

        #[rstest]
        #[case::call_closure(
            // Represents `함수 { 1 }()`.
            mkast!(prog loc str_loc!("", "함수 { 1 }()"), vec![
                mkast!(call loc str_loc!("", "함수 { 1 }()"),
                    target mkast!(closure loc str_loc!("", "함수 { 1 }"),
                        params vec![],
                        body vec![
                            mkast!(num 1.0, loc str_loc!("함수 { ", "1")),
                        ],
                    ),
                    args vec![],
                ),
            ]),
            root_empty_env(),
            mkval!(ValueKind::Number(1.0), str_loc!("", "함수 { 1 }()")),
        )]
        #[case::call_call(
            // Represents `함수 { 함수 { 1 } }()()`.
            mkast!(prog loc str_loc!("", "함수 { 함수 { 1 } }()()"), vec![
                mkast!(call loc str_loc!("", "함수 { 함수 { 1 } }()()"),
                    target mkast!(call loc str_loc!("", "함수 { 함수 { 1 } }()"),
                        target mkast!(closure loc str_loc!("", "함수 { 함수 { 1 } }"),
                            params vec![],
                            body vec![
                                mkast!(closure loc str_loc!("함수 { ", "함수 { 1 }"),
                                    params vec![],
                                    body vec![
                                        mkast!(num 1.0, loc str_loc!("함수 { 함수 { ", "1")),
                                    ]
                                ),
                            ],
                        ),
                        args vec![],
                    ),
                    args vec![],
                ),
            ]),
            root_empty_env(),
            mkval!(ValueKind::Number(1.0), str_loc!("", "함수 { 함수 { 1 } }()()")),
        )]
        #[case::call_id(
            // Represents `사과()`.
            mkast!(prog loc str_loc!("", "사과()"), vec![
                mkast!(call loc str_loc!("", "사과()"),
                    target mkast!(identifier "사과", loc str_loc!("", "사과")),
                    args vec![],
                ),
            ]),
            // Represents a binding for `사과` to `함수 {1}`.
            root_env("사과", &Value::new(ValueKind::Closure {
                parameters: vec![],
                body: vec![
                    mkast!(num 1.0, loc range()),
                ],
                env: Env::new(),
            }, range())),
            mkval!(ValueKind::Number(1.0), str_loc!("", "사과()")),
        )]
        #[case::call_with_args(
            // Represents `사과(1, 2)`.
            mkast!(prog loc str_loc!("", "사과(1, 2)"), vec![
                mkast!(call loc str_loc!("", "사과(1, 2)"),
                    target mkast!(identifier "사과", loc str_loc!("", "사과")),
                    args vec![
                        mkast!(num 1.0, loc str_loc!("사과(", "1")),
                        mkast!(num 2.0, loc str_loc!("사과(1, ", "2")),
                    ],
                ),
            ]),
            // Represents a binding for `사과` to `함수 오렌지, 바나나 {오렌지+바나나}`.
            root_env("사과", &Value::new(ValueKind::Closure {
                parameters: vec![
                    String::from("오렌지"),
                    String::from("바나나"),
                ],
                body: vec![
                    mkast!(infix InfixPlus, loc range(),
                        left mkast!(identifier "오렌지", loc range()),
                        right mkast!(identifier "바나나", loc range()),
                    ),
                ],
                env: Env::new(),
            }, range())),
            mkval!(ValueKind::Number(3.0), str_loc!("", "사과(1, 2)")),
        )]
        fn call(#[case] ast: Box<Ast>, #[case] mut env: Env, #[case] expected: Value) {
            assert_eval!(&ast, &mut env, expected);
        }

        #[rstest]
        #[case::call_num(
            // Represents `1()`.
            mkast!(prog loc str_loc!("", "1()"), vec![
                mkast!(call loc str_loc!("", "1()"),
                    target mkast!(num 1.0, loc str_loc!("", "1")),
                    args vec![],
                ),
            ]),
            root_empty_env(),
            mkerr!(InvalidCallTarget, str_loc!("", "1")),
        )]
        #[case::call_bool(
            // Represents `참()`.
            mkast!(prog loc str_loc!("", "참()"), vec![
                mkast!(call loc str_loc!("", "참()"),
                    target mkast!(boolean true, loc str_loc!("", "참")),
                    args vec![],
                ),
            ]),
            root_empty_env(),
            mkerr!(InvalidCallTarget, str_loc!("", "참")),
        )]
        fn invalid_call(#[case] ast: Box<Ast>, #[case] mut env: Env, #[case] error: EvalError) {
            assert_eval_fail!(&ast, &mut env, error);
        }
    }

    mod identifier {
        use super::*;

        #[rstest]
        #[case::num(
            // Represents `사과`.
            mkast!(prog loc str_loc!("", "사과"), vec![
                mkast!(identifier "사과", loc str_loc!("", "사과")),
            ]),
            // Represents a binding for `사과` to `1.0`.
            root_env("사과", &mkval!(ValueKind::Number(1.0), range())),
            mkval!(ValueKind::Number(1.0), str_loc!("", "사과")),
        )]
        #[case::bool(
            // Represents `사과`.
            mkast!(prog loc str_loc!("", "사과"), vec![
                mkast!(identifier "사과", loc str_loc!("", "사과")),
            ]),
            // Represents a binding for `사과` to `참`.
            root_env("사과", &mkval!(ValueKind::Bool(true), range())),
            mkval!(ValueKind::Bool(true), str_loc!("", "사과")),
        )]
        #[case::closure(
            // Represents `사과`.
            mkast!(prog loc str_loc!("", "사과"), vec![
                mkast!(identifier "사과", loc str_loc!("", "사과")),
            ]),
            root_env("사과", &Value::new(ValueKind::Closure {
                parameters: vec![String::from("오렌지")],
                body: vec![
                    mkast!(num 1.0, loc range()),
                ],
                env: Env::new()
            }, range())),
            Value::new(ValueKind::Closure {
                parameters: vec![String::from("오렌지")],
                body: vec![
                    mkast!(num 1.0, loc range()),
                ],
                env: Env::new()
            }, str_loc!("", "사과"))
        )]
        fn single_identifier(#[case] ast: Box<Ast>, #[case] mut env: Env, #[case] expected: Value) {
            assert_eval!(&ast, &mut env, expected);
        }

        #[rstest]
        #[case::undefined(
            // Represents `사과`.
            mkast!(prog loc str_loc!("", "사과"), vec![
                mkast!(identifier "사과", loc str_loc!("", "사과")),
            ]),
            mkerr!(UndefinedIdentifier, str_loc!("", "사과")),
        )]
        fn single_undefined_identifier(#[case] ast: Box<Ast>, #[case] error: EvalError) {
            assert_eval_fail!(&ast, error);
        }
    }

    mod leaf {
        use super::*;

        #[rstest]
        #[case::num(
            // Represents `1`.
            mkast!(prog loc str_loc!("", "1"), vec![
                mkast!(num 1.0, loc str_loc!("", "1")),
            ]),
            mkval!(ValueKind::Number(1.0), str_loc!("", "1"))
        )]
        #[case::bool(
            // Represents `참`.
            mkast!(prog loc str_loc!("", "참"), vec![
                mkast!(boolean true, loc str_loc!("", "참")),
            ]),
            mkval!(ValueKind::Bool(true), str_loc!("", "참"))
        )]
        #[case::str_without_interpolation(
            // Represents `"사과"`.
            mkast!(prog loc str_loc!("", "\"사과\""), vec![
                mkast!(string loc str_loc!("", "\"사과\""), vec![
                    mkstrseg!(Str, "사과", str_loc!("\"", "사과")),
                ]),
            ]),
            mkval!(ValueKind::Str(String::from("사과")), str_loc!("", "\"사과\""))
        )]
        #[case::closure(
            // Represents `함수 사과, 오렌지, 바나나 { 1 2 3 }`.
            mkast!(prog loc str_loc!("", "함수 사과, 오렌지, 바나나 { 1 2 3 }"), vec![
                mkast!(closure loc str_loc!("", "함수 사과, 오렌지, 바나나 { 1 2 3 }"),
                    params vec![
                        String::from("사과"),
                        String::from("오렌지"),
                        String::from("바나나"),
                    ],
                    body vec![
                        mkast!(num 1.0, loc str_loc!("함수 ", "사과")),
                        mkast!(num 2.0, loc str_loc!("함수 사과, ", "오렌지")),
                        mkast!(num 3.0, loc str_loc!("함수 사과, 오렌지, ", "바나나")),
                    ],
                ),
            ]),
            Value::new(ValueKind::Closure {
                parameters: vec![String::from("사과"), String::from("오렌지"), String::from("바나나")],
                body: vec![
                    mkast!(num 1.0, loc str_loc!("함수 ", "사과")),
                    mkast!(num 2.0, loc str_loc!("함수 사과, ", "오렌지")),
                    mkast!(num 3.0, loc str_loc!("함수 사과, 오렌지, ", "바나나")),
                ],
                env: Env::new()
            }, str_loc!("", "함수 사과, 오렌지, 바나나 { 1 2 3 }"))
        )]
        #[case::closure_with_closure(
            // Represents `함수 사과 { 함수 오렌지 { 사과 + 오렌지 } }`.
            mkast!(prog loc str_loc!("", "함수 사과 { 함수 오렌지 { 사과 + 오렌지 } }"), vec![
                mkast!(closure loc str_loc!("", "함수 사과 { 함수 오렌지 { 사과 + 오렌지 } }"),
                    params vec![
                        String::from("사과"),
                    ],
                    body vec![
                        mkast!(closure loc str_loc!("함수 사과 { ", "함수 오렌지 { 사과 + 오렌지 }"),
                            params vec![String::from("오렌지")],
                            body vec![
                                mkast!(infix InfixPlus, loc str_loc!("함수 사과 { 함수 오렌지 { ", "사과 + 오렌지"),
                                    left mkast!(identifier "사과", loc str_loc!("함수 사과 { 함수 오렌지 { ", "사과")),
                                    right mkast!(identifier "오렌지", loc str_loc!("함수 사과 { 함수 오렌지 { 사과 + ", "오렌지")),
                                ),
                            ],
                        ),
                    ],
                ),
            ]),
            Value::new(ValueKind::Closure {
                parameters: vec![
                    String::from("사과"),
                ],
                body: vec![
                    // Should contain the same AST with the closure body
                    mkast!(closure loc str_loc!("함수 사과 { ", "함수 오렌지 { 사과 + 오렌지 }"),
                        params vec![
                            String::from("오렌지"),
                        ],
                        body vec![
                            mkast!(infix InfixPlus, loc str_loc!("함수 사과 { 함수 오렌지 { ", "사과 + 오렌지"),
                                left mkast!(identifier "사과", loc str_loc!("함수 사과 { 함수 오렌지 { ", "사과")),
                                right mkast!(identifier "오렌지", loc str_loc!("함수 사과 { 함수 오렌지 { 사과 + ", "오렌지")),
                            ),
                        ],
                    ),
                ],
                env: Env::new()
            }, str_loc!("", "함수 사과 { 함수 오렌지 { 사과 + 오렌지 } }"))
        )]
        fn single(#[case] ast: Box<Ast>, #[case] expected: Value) {
            assert_eval!(&ast, expected);
        }

        #[rstest]
        #[case::num_id(
            // Represents `"{사과}"`.
            mkast!(prog loc str_loc!("", "\"{사과}\""), vec![
                mkast!(string loc str_loc!("", "\"{사과}\""), vec![
                    mkstrseg!(Identifier, "사과", str_loc!("\"", "사과")),
                ]),
            ]),
            // Represents a binding for `사과` to `12.25`.
            root_env("사과", &mkval!(ValueKind::Number(12.25), range())),
            mkval!(ValueKind::Str(String::from("12.25")), str_loc!("", "\"{사과}\""))
        )]
        #[case::bool_id(
            // Represents `"{사과}"`.
            mkast!(prog loc str_loc!("", "\"{사과}\""), vec![
                mkast!(string loc str_loc!("", "\"{사과}\""), vec![
                    mkstrseg!(Identifier, "사과", str_loc!("\"", "사과")),
                ]),
            ]),
            // Represents a binding for `사과` to `참`.
            root_env("사과", &mkval!(ValueKind::Bool(true), range())),
            mkval!(ValueKind::Str(String::from("참")), str_loc!("", "\"{사과}\""))
        )]
        #[case::str_id(
            // Represents `"{사과}"`.
            mkast!(prog loc str_loc!("", "\"{사과}\""), vec![
                mkast!(string loc str_loc!("", "\"{사과}\""), vec![
                    mkstrseg!(Identifier, "사과", str_loc!("\"", "사과")),
                ]),
            ]),
            // Represents a binding for `사과` to `"오렌지"`.
            root_env("사과", &mkval!(ValueKind::Str(String::from("오렌지")), range())),
            mkval!(ValueKind::Str(String::from("오렌지")), str_loc!("", "\"{사과}\""))
        )]
        fn string_interpolation(#[case] ast: Box<Ast>, #[case] mut env: Env, #[case] expected: Value) {
            assert_eval!(&ast, &mut env, expected);
        }
    }

    mod prefix {
        use super::*;

        #[rstest]
        #[case::plus_prefix(
            // Represents `+1`
            mkast!(prog loc str_loc!("", "+1"), vec![
                mkast!(prefix PrefixPlus, loc str_loc!("", "+1"),
                    operand mkast!(num 1.0, loc str_loc!("+", "1")),
                ),
            ]),
            mkval!(ValueKind::Number(1.0), str_loc!("", "+1"))
        )]
        #[case::minus_prefix(
            // Represents `-1`
            mkast!(prog loc str_loc!("", "-1"), vec![
                mkast!(prefix PrefixMinus, loc str_loc!("", "-1"),
                    operand mkast!(num 1.0, loc str_loc!("-", "1")),
                ),
            ]),
            mkval!(ValueKind::Number(-1.0), str_loc!("", "-1"))
        )]
        #[case::two_plus_prefixes(
            // Represents `++1`
            mkast!(prog loc str_loc!("", "++1"), vec![
                mkast!(prefix PrefixPlus, loc str_loc!("", "++1"),
                    operand mkast!(prefix PrefixPlus, loc str_loc!("+", "+1"),
                        operand mkast!(num 1.0, loc str_loc!("++", "1")),
                    ),
                ),
            ]),
            mkval!(ValueKind::Number(1.0), str_loc!("", "++1"))
        )]
        #[case::two_minus_prefixes(
            // Represents `--1`
            mkast!(prog loc str_loc!("", "--1"), vec![
                mkast!(prefix PrefixMinus, loc str_loc!("", "--1"),
                    operand mkast!(prefix PrefixMinus, loc str_loc!("-", "-1"),
                        operand mkast!(num 1.0, loc str_loc!("--", "1")),
                    ),
                ),
            ]),
            mkval!(ValueKind::Number(1.0), str_loc!("", "--1"))
        )]
        fn num_prefix(#[case] ast: Box<Ast>, #[case] expected: Value) {
            assert_eval!(&ast, expected);
        }

        #[rstest]
        #[case::bang_bool(
            // Represents `!참`
            mkast!(prog loc str_loc!("", "!참"), vec![
                mkast!(prefix PrefixBang, loc str_loc!("", "!참"),
                    operand mkast!(boolean true, loc str_loc!("!", "참")),
                ),
            ]),
            mkval!(ValueKind::Bool(false), str_loc!("", "!참"))
        )]
        #[case::two_bangs_bool(
            // Represents `!!참`
            mkast!(prog loc str_loc!("", "!!참"), vec![
                mkast!(prefix PrefixBang, loc str_loc!("", "!!참"),
                    operand mkast!(prefix PrefixBang, loc str_loc!("!", "!참"),
                        operand mkast!(boolean true, loc str_loc!("!!", "참")),
                    ),
                ),
            ]),
            mkval!(ValueKind::Bool(true), str_loc!("", "!!참"))
        )]
        fn bool_prefix(#[case] ast: Box<Ast>, #[case] expected: Value) {
            assert_eval!(&ast, expected);
        }

        #[rstest]
        #[case::plus_bool(
            // Represents `+참`
            mkast!(prog loc str_loc!("", "+참"), vec![
                mkast!(prefix PrefixPlus, loc str_loc!("", "+참"),
                    operand mkast!(boolean true, loc str_loc!("+", "참")),
                ),
            ]),
            mkerr!(NonNumPrefixOperand, str_loc!("+", "참")),
        )]
        #[case::minus_bool(
            // Represents `-참`
            mkast!(prog loc str_loc!("", "-참"), vec![
                mkast!(prefix PrefixMinus, loc str_loc!("", "-참"),
                    operand mkast!(boolean true, loc str_loc!("-", "참")),
                ),
            ]),
            mkerr!(NonNumPrefixOperand, str_loc!("-", "참")),
        )]
        #[case::bang_num(
            // Represents `!1`
            mkast!(prog loc str_loc!("", "!1"), vec![
                mkast!(prefix PrefixBang, loc str_loc!("", "!1"),
                    operand mkast!(num 1.0, loc str_loc!("!", "1")),
                ),
            ]),
            mkerr!(NonBoolPrefixOperand, str_loc!("!", "1")),
        )]
        fn wrong_type_prefix(#[case] ast: Box<Ast>, #[case] error: EvalError) {
            assert_eval_fail!(&ast, error);
        }
    }

    mod combinator_infix {
        use super::*;

        #[rstest]
        #[case::addition(
            // Represents `6+4`.
            mkast!(prog loc str_loc!("", "6+4"), vec![
                mkast!(infix InfixPlus, loc str_loc!("", "6+4"),
                    left mkast!(num 6.0, loc str_loc!("", "6")),
                    right mkast!(num 4.0, loc str_loc!("6+", "4")),
                ),
            ]),
            mkval!(ValueKind::Number(10.0), str_loc!("", "6+4"))
        )]
        #[case::subtraction(
            // Represents `6-4`.
            mkast!(prog loc str_loc!("", "6-4"), vec![
                mkast!(infix InfixMinus, loc str_loc!("", "6-4"),
                    left mkast!(num 6.0, loc str_loc!("", "6")),
                    right mkast!(num 4.0, loc str_loc!("6-", "4")),
                ),
            ]),
            mkval!(ValueKind::Number(2.0), str_loc!("", "6-4"))
        )]
        #[case::multiplication(
            // Represents `6*4`.
            mkast!(prog loc str_loc!("", "6*4"), vec![
                mkast!(infix InfixAsterisk, loc str_loc!("", "6*4"),
                    left mkast!(num 6.0, loc str_loc!("", "6")),
                    right mkast!(num 4.0, loc str_loc!("6*", "4")),
                ),
            ]),
            mkval!(ValueKind::Number(24.0), str_loc!("", "6*4"))
        )]
        #[case::division(
            // Represents `6/4`.
            mkast!(prog loc str_loc!("", "6/4"), vec![
                mkast!(infix InfixSlash, loc str_loc!("", "6/4"),
                    left mkast!(num 6.0, loc str_loc!("", "6")),
                    right mkast!(num 4.0, loc str_loc!("6/", "4")),
                ),
            ]),
            mkval!(ValueKind::Number(1.5), str_loc!("", "6/4"))
        )]
        #[case::modular(
            // Represents `6%4`.
            mkast!(prog loc str_loc!("", "6%4"), vec![
                mkast!(infix InfixPercent, loc str_loc!("", "6%4"),
                    left mkast!(num 6.0, loc str_loc!("", "6")),
                    right mkast!(num 4.0, loc str_loc!("6%", "4")),
                ),
            ]),
            mkval!(ValueKind::Number(2.0), str_loc!("", "6%4"))
        )]
        fn arithmetic_infix_nums(#[case] ast: Box<Ast>, #[case] expected: Value) {
            assert_eval!(&ast, expected);
        }

        #[rstest]
        #[case::addition(
            // Represents `"사과" + "오렌지"`.
            mkast!(prog loc str_loc!("", "\"사과\" + \"오렌지\""), vec![
                mkast!(infix InfixPlus, loc str_loc!("", "\"사과\" + \"오렌지\""),
                    left mkast!(string loc str_loc!("", "\"사과\""), vec![
                        mkstrseg!(Str, "사과", str_loc!("\"", "사과")),
                    ]),
                    right mkast!(string loc str_loc!("\"사과\" + ", "\"오렌지\""), vec![
                        mkstrseg!(Str, "오렌지", str_loc!("\"사과\" = \"", "오렌지")),
                    ]),
                ),
            ]),
            mkval!(ValueKind::Str(String::from("사과오렌지")), str_loc!("", "\"사과\" + \"오렌지\""))
        )]
        #[case::multiplication(
            // Represents `"사과" * 3`.
            mkast!(prog loc str_loc!("", "\"사과\" * 3"), vec![
                mkast!(infix InfixAsterisk, loc str_loc!("", "\"사과\" * 3"),
                    left mkast!(string loc str_loc!("", "\"사과\""), vec![
                        mkstrseg!(Str, "사과", str_loc!("\"", "사과")),
                    ]),
                    right mkast!(num 3.0, loc str_loc!("\"사과\" * ", "3")),
                ),
            ]),
            mkval!(ValueKind::Str(String::from("사과사과사과")), str_loc!("", "\"사과\" * 3"))
        )]
        fn arithmetic_infix_strs(#[case] ast: Box<Ast>, #[case] expected: Value) {
            assert_eval!(&ast, expected);
        }

        #[rstest]
        #[case::five_kinds(
            // Represents `9 * 8 % 7 - 6 + 5 / 4`, which is parsed into `(((9 * 8) % 7) - 6) + (5 / 4)`.
            // Note that the associativity of an expression is determined in the parsing step, as represented in the AST result.
            mkast!(prog loc str_loc!("", "9 * 8 % 7 - 6 + 5 / 4"), vec![
                mkast!(infix InfixPlus, loc str_loc!("", "9 * 8 % 7 - 6 + 5 / 4"),
                    left mkast!(infix InfixMinus, loc str_loc!("", "9 * 8 % 7 - 6"),
                        left mkast!(infix InfixPercent, loc str_loc!("", "9 * 8 % 7"),
                            left mkast!(infix InfixAsterisk, loc str_loc!("", "9 * 8"),
                                left mkast!(num 9.0, loc str_loc!("", "9")),
                                right mkast!(num 8.0, loc str_loc!("9 * ", "8")),
                            ),
                            right mkast!(num 7.0, loc str_loc!("9 * 8 %", "7")),
                        ),
                        right mkast!(num 6.0, loc str_loc!("9 * 8 % 7 - ", "6")),
                    ),
                    right mkast!(infix InfixSlash, loc str_loc!("9 * 8 % 7 - 6 + ", "5 / 4"),
                        left mkast!(num 5.0, loc str_loc!("9 * 8 % 7 - 6 + ", "5")),
                        right mkast!(num 4.0, loc str_loc!("9 * 8 % 7 - 6 + 5 / ", "4")),
                    ),
                ),
            ]),
            mkval!(ValueKind::Number(-2.75), str_loc!("", "9 * 8 % 7 - 6 + 5 / 4"))
        )]
        fn arithmetic_compound(#[case] ast: Box<Ast>, #[case] expected: Value) {
            assert_eval!(&ast, expected);
        }

        #[rstest]
        #[case::conjunction_on_true_true(
            // Represents `참 그리고 참`.
            mkast!(prog loc str_loc!("", "참 그리고 참"), vec![
                mkast!(infix InfixConjunct, loc str_loc!("", "참 그리고 참"),
                    left mkast!(boolean true, loc str_loc!("", "참")),
                    right mkast!(boolean true, loc str_loc!("참 그리고 ", "참")),
                ),
            ]),
            mkval!(ValueKind::Bool(true), str_loc!("", "참 그리고 참"))
        )]
        #[case::conjunction_on_true_false(
            // Represents `참 그리고 거짓`.
            mkast!(prog loc str_loc!("", "참 그리고 거짓"), vec![
                mkast!(infix InfixConjunct, loc str_loc!("", "참 그리고 거짓"),
                    left mkast!(boolean true, loc str_loc!("", "참")),
                    right mkast!(boolean false, loc str_loc!("참 그리고 ", "거짓")),
                ),
            ]),
            mkval!(ValueKind::Bool(false), str_loc!("", "참 그리고 거짓"))
        )]
        #[case::conjunction_on_false_true(
            // Represents `거짓 그리고 참`.
            mkast!(prog loc str_loc!("", "거짓 그리고 참"), vec![
                mkast!(infix InfixConjunct, loc str_loc!("", "거짓 그리고 참"),
                    left mkast!(boolean false, loc str_loc!("", "거짓")),
                    right mkast!(boolean true, loc str_loc!("거짓 그리고 ", "참")),
                ),
            ]),
            mkval!(ValueKind::Bool(false), str_loc!("", "거짓 그리고 참"))
        )]
        #[case::conjunction_on_false_false(
            // Represents `거짓 그리고 거짓`.
            mkast!(prog loc str_loc!("", "거짓 그리고 거짓"), vec![
                mkast!(infix InfixConjunct, loc str_loc!("", "거짓 그리고 거짓"),
                    left mkast!(boolean false, loc str_loc!("", "거짓")),
                    right mkast!(boolean false, loc str_loc!("거짓 그리고 ", "거짓")),
                ),
            ]),
            mkval!(ValueKind::Bool(false), str_loc!("", "거짓 그리고 거짓"))
        )]
        #[case::disjunction_on_true_true(
            // Represents `참 또는 참`.
            mkast!(prog loc str_loc!("", "참 또는 참"), vec![
                mkast!(infix InfixDisjunct, loc str_loc!("", "참 또는 참"),
                    left mkast!(boolean true, loc str_loc!("", "참")),
                    right mkast!(boolean true, loc str_loc!("참 또는 ", "참")),
                ),
            ]),
            mkval!(ValueKind::Bool(true), str_loc!("", "참 또는 참"))
        )]
        #[case::disjunction_on_true_false(
            // Represents `참 또는 거짓`.
            mkast!(prog loc str_loc!("", "참 또는 거짓"), vec![
                mkast!(infix InfixDisjunct, loc str_loc!("", "참 또는 거짓"),
                    left mkast!(boolean true, loc str_loc!("", "참")),
                    right mkast!(boolean false, loc str_loc!("참 또는 ", "거짓")),
                ),
            ]),
            mkval!(ValueKind::Bool(true), str_loc!("", "참 또는 거짓"))
        )]
        #[case::disjunction_on_false_true(
            // Represents `거짓 또는 참`.
            mkast!(prog loc str_loc!("", "거짓 또는 참"), vec![
                mkast!(infix InfixDisjunct, loc str_loc!("", "거짓 또는 참"),
                    left mkast!(boolean false, loc str_loc!("", "거짓")),
                    right mkast!(boolean true, loc str_loc!("거짓 또는 ", "참")),
                ),
            ]),
            mkval!(ValueKind::Bool(true), str_loc!("", "거짓 또는 참"))
        )]
        #[case::disjunction_on_false_false(
            // Represents `거짓 또는 거짓`.
            mkast!(prog loc str_loc!("", "거짓 또는 거짓"), vec![
                mkast!(infix InfixDisjunct, loc str_loc!("", "거짓 또는 거짓"),
                    left mkast!(boolean false, loc str_loc!("", "거짓")),
                    right mkast!(boolean false, loc str_loc!("거짓 또는 ", "거짓")),
                ),
            ]),
            mkval!(ValueKind::Bool(false), str_loc!("", "거짓 또는 거짓"))
        )]
        fn connective_infix(#[case] ast: Box<Ast>, #[case] expected: Value) {
            assert_eval!(&ast, expected);
        }

        #[rstest]
        #[case::left_bool_addition(
            // Represents `참 + 1`.
            mkast!(prog loc str_loc!("", "참 + 1"), vec![
                mkast!(infix InfixPlus, loc str_loc!("", "참 + 1"),
                    left mkast!(boolean true, loc str_loc!("", "참")),
                    right mkast!(num 1.0, loc str_loc!("참 + ", "1")),
                ),
            ]),
            mkerr!(NonNumOrStrInfixLeftOperand, str_loc!("", "참")),
        )]
        #[case::right_bool_addition(
            // Represents `1 + 참`.
            mkast!(prog loc str_loc!("", "1 + 참"), vec![
                mkast!(infix InfixPlus, loc str_loc!("", "1 + 참"),
                    left mkast!(num 1.0, loc str_loc!("", "1")),
                    right mkast!(boolean true, loc str_loc!("1 + ", "참")),
                ),
            ]),
            mkerr!(NonNumInfixRightOperand, str_loc!("1 + ", "참")),
        )]
        #[case::left_bool_subtraction(
            // Represents `참 - 1`.
            mkast!(prog loc str_loc!("", "참 - 1"), vec![
                mkast!(infix InfixMinus, loc str_loc!("", "참 - 1"),
                    left mkast!(boolean true, loc str_loc!("", "참")),
                    right mkast!(num 1.0, loc str_loc!("참 - ", "1")),
                ),
            ]),
            mkerr!(NonNumInfixLeftOperand, str_loc!("", "참")),
        )]
        #[case::right_bool_subtraction(
            // Represents `1 - 참`.
            mkast!(prog loc str_loc!("", "1 - 참"), vec![
                mkast!(infix InfixMinus, loc str_loc!("", "1 - 참"),
                    left mkast!(num 1.0, loc str_loc!("", "1")),
                    right mkast!(boolean true, loc str_loc!("1 - ", "참")),
                ),
            ]),
            mkerr!(NonNumInfixRightOperand, str_loc!("1 - ", "참")),
        )]
        #[case::left_bool_multiplication(
            // Represents `참 * 1`.
            mkast!(prog loc str_loc!("", "참 * 1"), vec![
                mkast!(infix InfixAsterisk, loc str_loc!("", "참 * 1"),
                    left mkast!(boolean true, loc str_loc!("", "참")),
                    right mkast!(num 1.0, loc str_loc!("참 * ", "1")),
                ),
            ]),
            mkerr!(NonNumOrStrInfixLeftOperand, str_loc!("", "참")),
        )]
        #[case::right_bool_multiplication(
            // Represents `1 * 참`.
            mkast!(prog loc str_loc!("", "1 * 참"), vec![
                mkast!(infix InfixAsterisk, loc str_loc!("", "1 * 참"),
                    left mkast!(num 1.0, loc str_loc!("", "1")),
                    right mkast!(boolean true, loc str_loc!("1 * ", "참")),
                ),
            ]),
            mkerr!(NonNumInfixRightOperand, str_loc!("1 * ", "참")),
        )]
        #[case::left_bool_division(
            // Represents `참 / 1`.
            mkast!(prog loc str_loc!("", "참 / 1"), vec![
                mkast!(infix InfixSlash, loc str_loc!("", "참 / 1"),
                    left mkast!(boolean true, loc str_loc!("", "참")),
                    right mkast!(num 1.0, loc str_loc!("참 / ", "1")),
                ),
            ]),
            mkerr!(NonNumInfixLeftOperand, str_loc!("", "참")),
        )]
        #[case::right_bool_division(
            // Represents `1 / 참`.
            mkast!(prog loc str_loc!("", "1 / 참"), vec![
                mkast!(infix InfixSlash, loc str_loc!("", "1 / 참"),
                    left mkast!(num 1.0, loc str_loc!("", "1")),
                    right mkast!(boolean true, loc str_loc!("1 / ", "참")),
                ),
            ]),
            mkerr!(NonNumInfixRightOperand, str_loc!("1 / ", "참")),
        )]
        #[case::left_bool_modular(
            // Represents `참 % 1`.
            mkast!(prog loc str_loc!("", "참 % 1"), vec![
                mkast!(infix InfixPercent, loc str_loc!("", "참 % 1"),
                    left mkast!(boolean true, loc str_loc!("", "참")),
                    right mkast!(num 1.0, loc str_loc!("참 % ", "1")),
                ),
            ]),
            mkerr!(NonNumInfixLeftOperand, str_loc!("", "참")),
        )]
        #[case::right_bool_modular(
            // Represents `1 % 참`.
            mkast!(prog loc str_loc!("", "1 % 참"), vec![
                mkast!(infix InfixPercent, loc str_loc!("", "1 % 참"),
                    left mkast!(num 1.0, loc str_loc!("", "1")),
                    right mkast!(boolean true, loc str_loc!("1 % ", "참")),
                ),
            ]),
            mkerr!(NonNumInfixRightOperand, str_loc!("1 % ", "참")),
        )]
        fn arithmetic_infix_with_wrong_type_operand(#[case] ast: Box<Ast>, #[case] error: EvalError) {
            assert_eval_fail!(&ast, error);
        }

        #[rstest]
        #[case::left_num_conjunction(
            // Represents `1 그리고 참`.
            mkast!(prog loc str_loc!("", "1 그리고 참"), vec![
                mkast!(infix InfixConjunct, loc str_loc!("", "1 그리고 참"),
                    left mkast!(num 1.0, loc str_loc!("", "1")),
                    right mkast!(boolean true, loc str_loc!("1 그리고 ", "참")),
                ),
            ]),
            mkerr!(NonBoolInfixOperand, str_loc!("", "1")),
        )]
        #[case::right_num_conjunction(
            // Represents `참 그리고 1`.
            mkast!(prog loc str_loc!("", "참 그리고 1"), vec![
                mkast!(infix InfixConjunct, loc str_loc!("", "참 그리고 1"),
                    left mkast!(boolean true, loc str_loc!("", "참")),
                    right mkast!(num 1.0, loc str_loc!("참 그리고 ", "1")),
                ),
            ]),
            mkerr!(NonBoolInfixOperand, str_loc!("참 그리고 ", "1")),
        )]
        #[case::left_num_disjunction(
            // Represents `1 또는 참`.
            mkast!(prog loc str_loc!("", "1 또는 참"), vec![
                mkast!(infix InfixDisjunct, loc str_loc!("", "1 또는 참"),
                    left mkast!(num 1.0, loc str_loc!("", "1")),
                    right mkast!(boolean true, loc str_loc!("1 또는 ", "참")),
                ),
            ]),
            mkerr!(NonBoolInfixOperand, str_loc!("", "1")),
        )]
        #[case::right_num_disjunction(
            // Represents `참 또는 1`.
            mkast!(prog loc str_loc!("", "참 또는 1"), vec![
                mkast!(infix InfixConjunct, loc str_loc!("", "참 또는 1"),
                    left mkast!(boolean true, loc str_loc!("", "참")),
                    right mkast!(num 1.0, loc str_loc!("참 또는 ", "1")),
                ),
            ]),
            mkerr!(NonBoolInfixOperand, str_loc!("참 또는 ", "1")),
        )]
        fn connective_infix_with_wrong_type_operand(#[case] ast: Box<Ast>, #[case] error: EvalError) {
            assert_eval_fail!(&ast, error);
        }
    }

    mod assignment_infix {
        use super::*;

        #[rstest]
        #[case::id_equals_num(
            // Represents `사과 = 1`.
            mkast!(prog loc str_loc!("", "사과 = 1"), vec![
                mkast!(infix InfixEquals, loc str_loc!("", "사과 = 1"),
                    left mkast!(identifier "사과", loc str_loc!("", "사과")),
                    right mkast!(num 1.0, loc str_loc!("사과 = ", "1")),
                ),
            ]),
            mkval!(ValueKind::Number(1.0), str_loc!("", "사과 = 1"))
        )]
        #[case::id_equals_bool(
            // Represents `사과 = 참`.
            mkast!(prog loc str_loc!("", "사과 = 참"), vec![
                mkast!(infix InfixEquals, loc str_loc!("", "사과 = 참"),
                    left mkast!(identifier "사과", loc str_loc!("", "사과")),
                    right mkast!(boolean true, loc str_loc!("사과 = ", "참")),
                ),
            ]),
            mkval!(ValueKind::Bool(true), str_loc!("", "사과 = 참"))
        )]
        fn equals(#[case] ast: Box<Ast>, #[case] expected: Value) {
            assert_eval!(&ast, expected);
        }

        #[rstest]
        #[case::id_plus_equals_num(
            // Represents `사과 += 4`.
            mkast!(prog loc str_loc!("", "사과 += 4"), vec![
                mkast!(infix InfixPlusEquals, loc str_loc!("", "사과 += 4"),
                    left mkast!(identifier "사과", loc str_loc!("", "사과")),
                    right mkast!(num 4.0, loc str_loc!("사과 += ", "4")),
                ),
            ]),
            // Represents a binding for `사과` to `6`.
            root_env("사과", &mkval!(ValueKind::Number(6.0), range())),
            mkval!(ValueKind::Number(10.0), str_loc!("", "사과 += 4"))
        )]
        #[case::id_minus_equals_num(
            // Represents `사과 -= 4`.
            mkast!(prog loc str_loc!("", "사과 -= 4"), vec![
                mkast!(infix InfixMinusEquals, loc str_loc!("", "사과 -= 4"),
                    left mkast!(identifier "사과", loc str_loc!("", "사과")),
                    right mkast!(num 4.0, loc str_loc!("사과 -= ", "4")),
                ),
            ]),
            // Represents a binding for `사과` to `6`.
            root_env("사과", &mkval!(ValueKind::Number(6.0), range())),
            mkval!(ValueKind::Number(2.0), str_loc!("", "사과 -= 4"))
        )]
        #[case::id_asterisk_equals_num(
            // Represents `사과 *= 4`.
            mkast!(prog loc str_loc!("", "사과 *= 4"), vec![
                mkast!(infix InfixAsteriskEquals, loc str_loc!("", "사과 *= 4"),
                    left mkast!(identifier "사과", loc str_loc!("", "사과")),
                    right mkast!(num 4.0, loc str_loc!("사과 *= ", "4")),
                ),
            ]),
            // Represents a binding for `사과` to `6`.
            root_env("사과", &mkval!(ValueKind::Number(6.0), range())),
            mkval!(ValueKind::Number(24.0), str_loc!("", "사과 *= 4"))
        )]
        #[case::id_slash_equals_num(
            // Represents `사과 /= 4`.
            mkast!(prog loc str_loc!("", "사과 /= 4"), vec![
                mkast!(infix InfixSlashEquals, loc str_loc!("", "사과 /= 4"),
                    left mkast!(identifier "사과", loc str_loc!("", "사과")),
                    right mkast!(num 4.0, loc str_loc!("사과 /= ", "4")),
                ),
            ]),
            // Represents a binding for `사과` to `6`.
            root_env("사과", &mkval!(ValueKind::Number(6.0), range())),
            mkval!(ValueKind::Number(1.5), str_loc!("", "사과 /= 4"))
        )]
        #[case::id_percent_equals_num(
            // Represents `사과 %= 4`.
            mkast!(prog loc str_loc!("", "사과 %= 4"), vec![
                mkast!(infix InfixPercentEquals, loc str_loc!("", "사과 %= 4"),
                    left mkast!(identifier "사과", loc str_loc!("", "사과")),
                    right mkast!(num 4.0, loc str_loc!("사과 %= ", "4")),
                ),
            ]),
            // Represents a binding for `사과` to `6`.
            root_env("사과", &mkval!(ValueKind::Number(6.0), range())),
            mkval!(ValueKind::Number(2.0), str_loc!("", "사과 %= 4"))
        )]
        fn combinating_equals(#[case] ast: Box<Ast>, #[case] mut env: Env, #[case] expected: Value) {
            assert_eval!(&ast, &mut env, expected);
        }

        #[rstest]
        #[case::num_id_plus_equals_bool(
            // Represents `사과 += 참`.
            mkast!(prog loc str_loc!("", "사과 += 참"), vec![
                mkast!(infix InfixPlusEquals, loc str_loc!("", "사과 += 참"),
                    left mkast!(identifier "사과", loc str_loc!("", "사과")),
                    right mkast!(boolean true, loc str_loc!("사과 += ", "참")),
                ),
            ]),
            // Represents a binding for `사과` to `1`.
            root_env("사과", &mkval!(ValueKind::Number(1.0), range())),
            mkerr!(NonNumInfixRightOperand, str_loc!("사과 += ", "참")),
        )]
        #[case::num_id_minus_equals_bool(
            // Represents `사과 -= 참`.
            mkast!(prog loc str_loc!("", "사과 += 참"), vec![
                mkast!(infix InfixMinusEquals, loc str_loc!("", "사과 += 참"),
                    left mkast!(identifier "사과", loc str_loc!("", "사과")),
                    right mkast!(boolean true, loc str_loc!("사과 += ", "참")),
                ),
            ]),
            // Represents a binding for `사과` to `1`.
            root_env("사과", &mkval!(ValueKind::Number(1.0), range())),
            mkerr!(NonNumInfixRightOperand, str_loc!("사과 -= ", "참")),
        )]
        #[case::num_id_asterisk_equals_bool(
            // Represents `사과 *= 참`.
            mkast!(prog loc str_loc!("", "사과 += 참"), vec![
                mkast!(infix InfixAsteriskEquals, loc str_loc!("", "사과 += 참"),
                    left mkast!(identifier "사과", loc str_loc!("", "사과")),
                    right mkast!(boolean true, loc str_loc!("사과 += ", "참")),
                ),
            ]),
            // Represents a binding for `사과` to `1`.
            root_env("사과", &mkval!(ValueKind::Number(1.0), range())),
            mkerr!(NonNumInfixRightOperand, str_loc!("사과 *= ", "참")),
        )]
        #[case::num_id_slash_equals_bool(
            // Represents `사과 /= 참`.
            mkast!(prog loc str_loc!("", "사과 += 참"), vec![
                mkast!(infix InfixSlashEquals, loc str_loc!("", "사과 += 참"),
                    left mkast!(identifier "사과", loc str_loc!("", "사과")),
                    right mkast!(boolean true, loc str_loc!("사과 += ", "참")),
                ),
            ]),
            // Represents a binding for `사과` to `1`.
            root_env("사과", &mkval!(ValueKind::Number(1.0), range())),
            mkerr!(NonNumInfixRightOperand, str_loc!("사과 /= ", "참")),
        )]
        #[case::num_id_percent_equals_bool(
            // Represents `사과 %= 참`.
            mkast!(prog loc str_loc!("", "사과 += 참"), vec![
                mkast!(infix InfixPercentEquals, loc str_loc!("", "사과 += 참"),
                    left mkast!(identifier "사과", loc str_loc!("", "사과")),
                    right mkast!(boolean true, loc str_loc!("사과 += ", "참")),
                ),
            ]),
            // Represents a binding for `사과` to `1`.
            root_env("사과", &mkval!(ValueKind::Number(1.0), range())),
            mkerr!(NonNumInfixRightOperand, str_loc!("사과 %= ", "참")),
        )]
        fn combinating_equals_with_wrong_type(#[case] ast: Box<Ast>, #[case] mut env: Env, #[case] error: EvalError) {
            assert_eval_fail!(&ast, &mut env, error);
        }
    }

    mod comparison_infix {
        use super::*;

        #[rstest]
        #[case::equal_bool(
            // Represents `참 == 참`.
            mkast!(prog loc str_loc!("", "참 == 참"), vec![
                mkast!(infix InfixDoubleEquals, loc str_loc!("", "참 == 참"),
                    left mkast!(boolean true, loc str_loc!("", "참")),
                    right mkast!(boolean true, loc str_loc!("참 == ", "참")),
                ),
            ]),
            mkval!(ValueKind::Bool(true), str_loc!("", "참 == 참"))
        )]
        #[case::not_equal_bool(
            // Represents `참 == 거짓`.
            mkast!(prog loc str_loc!("", "참 == 거짓"), vec![
                mkast!(infix InfixDoubleEquals, loc str_loc!("", "참 == 거짓"),
                    left mkast!(boolean true, loc str_loc!("", "참")),
                    right mkast!(boolean false, loc str_loc!("참 == ", "거짓")),
                ),
            ]),
            mkval!(ValueKind::Bool(false), str_loc!("", "참 == 거짓"))
        )]
        #[case::equal_num(
            // Represents `1 == 1`.
            mkast!(prog loc str_loc!("", "1 == 1"), vec![
                mkast!(infix InfixDoubleEquals, loc str_loc!("", "1 == 1"),
                    left mkast!(num 1.0, loc str_loc!("", "1")),
                    right mkast!(num 1.0, loc str_loc!("1 == ", "1")),
                ),
            ]),
            mkval!(ValueKind::Bool(true), str_loc!("", "1 == 1"))
        )]
        #[case::not_equal_num(
            // Represents `1 == 2`.
            mkast!(prog loc str_loc!("", "1 == 2"), vec![
                mkast!(infix InfixDoubleEquals, loc str_loc!("", "1 == 2"),
                    left mkast!(num 1.0, loc str_loc!("", "1")),
                    right mkast!(num 2.0, loc str_loc!("1 == ", "2")),
                ),
            ]),
            mkval!(ValueKind::Bool(false), str_loc!("", "1 == 2"))
        )]
        #[case::equal_str(
            // Represents `"사과" == "사과"`.
            mkast!(prog loc str_loc!("", "\"사과\" == \"사과\""), vec![
                mkast!(infix InfixDoubleEquals, loc str_loc!("", "\"사과\" == \"사과\""),
                    left mkast!(string loc str_loc!("", "\"사과\""), vec![
                        mkstrseg!(Str, "사과", str_loc!("\"", "사과")),
                    ]),
                    right mkast!(string loc str_loc!("\"사과\" == ", "\"사과\""), vec![
                        mkstrseg!(Str, "사과", str_loc!("\"사과\" == \"", "사과")),
                    ]),
                ),
            ]),
            mkval!(ValueKind::Bool(true), str_loc!("", "\"사과\" == \"사과\""))
        )]
        #[case::not_equal_str(
            // Represents `"사과" == "바나나"`.
            mkast!(prog loc str_loc!("", "\"사과\" == \"바나나\""), vec![
                mkast!(infix InfixDoubleEquals, loc str_loc!("", "\"사과\" == \"바나나\""),
                    left mkast!(string loc str_loc!("", "\"사과\""), vec![
                        mkstrseg!(Str, "사과", str_loc!("\"", "사과")),
                    ]),
                    right mkast!(string loc str_loc!("\"사과\" == ", "\"바나나\""), vec![
                        mkstrseg!(Str, "바나나", str_loc!("\"사과\" == \"", "바나나")),
                    ]),
                ),
            ]),
            mkval!(ValueKind::Bool(false), str_loc!("", "\"사과\" == \"바나나\""))
        )]
        fn double_equals(#[case] ast: Box<Ast>, #[case] expected: Value) {
            assert_eval!(&ast, expected);
        }

        #[rstest]
        #[case::closure_and_num(
            // Represents `함수 { 1 } == 1`.
            mkast!(prog loc str_loc!("", "함수 { 1 } == 1"), vec![
                mkast!(infix InfixDoubleEquals, loc str_loc!("", "함수 { 1 } == 1"),
                    left mkast!(closure loc str_loc!("", "함수 { 1 }"),
                        params vec![],
                        body vec![
                            mkast!(num 1.0, loc str_loc!("함수 { ", "1"))
                        ],
                    ),
                    right mkast!(num 1.0, loc str_loc!("함수 { 1 } == ", "1")),
                ),
            ]),
            mkerr!(BadTypeEqLeftOperand, str_loc!("", "함수 { 1 }")),
        )]
        #[case::num_and_closure(
            // Represents `1 == 함수 { 1 }`.
            mkast!(prog loc str_loc!("", "1 == 함수 { 1 }"), vec![
                mkast!(infix InfixDoubleEquals, loc str_loc!("", "1 == 함수 { 1 }"),
                    left mkast!(num 1.0, loc str_loc!("", "1")),
                    right mkast!(closure loc str_loc!("1 == ", "함수 { 1 }"),
                        params vec![],
                        body vec![
                            mkast!(num 1.0, loc str_loc!("1 == 함수 { ", "1"))
                        ],
                    ),
                ),
            ]),
            mkerr!(BadTypeEqRightOperand, str_loc!("1 == ", "함수 { 1 }")),
        )]
        #[case::num_and_str(
            // Represents `1 == "사과"`.
            mkast!(prog loc str_loc!("", "1 == \"사과\""), vec![
                mkast!(infix InfixDoubleEquals, loc str_loc!("", "1 == \"사과\""),
                    left mkast!(num 1.0, loc str_loc!("", "1")),
                    right mkast!(string loc str_loc!("1 == ", "\"사과\""), vec![
                        mkstrseg!(Str, "사과", str_loc!("1 == \"", "사과")),
                    ]),
                ),
            ]),
            mkerr!(NotSameTypeInfixOperands, str_loc!("", "1 == \"사과\"")),
        )]
        #[case::num_and_bool(
            // Represents `1 == 참`.
            mkast!(prog loc str_loc!("", "1 == 참"), vec![
                mkast!(infix InfixDoubleEquals, loc str_loc!("", "1 == 참"),
                    left mkast!(num 1.0, loc str_loc!("", "1")),
                    right mkast!(boolean true, loc str_loc!("1 == ", "참")),
                ),
            ]),
            mkerr!(NotSameTypeInfixOperands, str_loc!("", "1 == 참")),
        )]
        fn wrong_type_double_equals(#[case] ast: Box<Ast>, #[case] error: EvalError) {
            assert_eval_fail!(&ast, error);
        }

        #[rstest]
        #[case::equal_bool(
            // Represents `참 != 참`.
            mkast!(prog loc str_loc!("", "참 != 참"), vec![
                mkast!(infix InfixBangEquals, loc str_loc!("", "참 != 참"),
                    left mkast!(boolean true, loc str_loc!("", "참")),
                    right mkast!(boolean true, loc str_loc!("참 != ", "참")),
                ),
            ]),
            mkval!(ValueKind::Bool(false), str_loc!("", "참 != 참"))
        )]
        #[case::not_equal_bool(
            // Represents `참 != 거짓`.
            mkast!(prog loc str_loc!("", "참 != 거짓"), vec![
                mkast!(infix InfixBangEquals, loc str_loc!("", "참 != 거짓"),
                    left mkast!(boolean true, loc str_loc!("", "참")),
                    right mkast!(boolean false, loc str_loc!("참 != ", "거짓")),
                ),
            ]),
            mkval!(ValueKind::Bool(true), str_loc!("", "참 != 거짓"))
        )]
        #[case::equal_num(
            // Represents `1 != 1`.
            mkast!(prog loc str_loc!("", "1 != 1"), vec![
                mkast!(infix InfixBangEquals, loc str_loc!("", "1 != 1"),
                    left mkast!(num 1.0, loc str_loc!("", "1")),
                    right mkast!(num 1.0, loc str_loc!("1 != ", "1")),
                ),
            ]),
            mkval!(ValueKind::Bool(false), str_loc!("", "1 != 1"))
        )]
        #[case::not_equal_num(
            // Represents `1 != 2`.
            mkast!(prog loc str_loc!("", "1 != 2"), vec![
                mkast!(infix InfixBangEquals, loc str_loc!("", "1 != 2"),
                    left mkast!(num 1.0, loc str_loc!("", "1")),
                    right mkast!(num 2.0, loc str_loc!("1 != ", "2")),
                ),
            ]),
            mkval!(ValueKind::Bool(true), str_loc!("", "1 != 2"))
        )]
        #[case::equal_str(
            // Represents `"사과" != "사과"`.
            mkast!(prog loc str_loc!("", "\"사과\" != \"사과\""), vec![
                mkast!(infix InfixBangEquals, loc str_loc!("", "\"사과\" != \"사과\""),
                    left mkast!(string loc str_loc!("", "\"사과\""), vec![
                        mkstrseg!(Str, "사과", str_loc!("\"", "사과")),
                    ]),
                    right mkast!(string loc str_loc!("\"사과\" != ", "\"사과\""), vec![
                        mkstrseg!(Str, "사과", str_loc!("\"사과\" != \"", "사과")),
                    ]),
                ),
            ]),
            mkval!(ValueKind::Bool(false), str_loc!("", "\"사과\" != \"사과\""))
        )]
        #[case::not_equal_str(
            // Represents `"사과" != "바나나"`.
            mkast!(prog loc str_loc!("", "\"사과\" != \"바나나\""), vec![
                mkast!(infix InfixBangEquals, loc str_loc!("", "\"사과\" != \"바나나\""),
                    left mkast!(string loc str_loc!("", "\"사과\""), vec![
                        mkstrseg!(Str, "사과", str_loc!("\"", "사과")),
                    ]),
                    right mkast!(string loc str_loc!("\"사과\" != ", "\"바나나\""), vec![
                        mkstrseg!(Str, "바나나", str_loc!("\"사과\" != \"", "바나나")),
                    ]),
                ),
            ]),
            mkval!(ValueKind::Bool(true), str_loc!("", "\"사과\" != \"바나나\""))
        )]
        fn bang_equals(#[case] ast: Box<Ast>, #[case] expected: Value) {
            assert_eval!(&ast, expected);
        }

        #[rstest]
        #[case::closure_and_num(
            // Represents `함수 { 1 } != 1`.
            mkast!(prog loc str_loc!("", "함수 { 1 } != 1"), vec![
                mkast!(infix InfixBangEquals, loc str_loc!("", "함수 { 1 } != 1"),
                    left mkast!(closure loc str_loc!("", "함수 { 1 }"),
                        params vec![],
                        body vec![
                            mkast!(num 1.0, loc str_loc!("함수 { ", "1"))
                        ],
                    ),
                    right mkast!(num 1.0, loc str_loc!("함수 { 1 } != ", "1")),
                ),
            ]),
            mkerr!(BadTypeEqLeftOperand, str_loc!("", "함수 { 1 }")),
        )]
        #[case::num_and_closure(
            // Represents `1 != 함수 { 1 }`.
            mkast!(prog loc str_loc!("", "1 != 함수 { 1 }"), vec![
                mkast!(infix InfixBangEquals, loc str_loc!("", "1 != 함수 { 1 }"),
                    left mkast!(num 1.0, loc str_loc!("", "1")),
                    right mkast!(closure loc str_loc!("1 != ", "함수 { 1 }"),
                        params vec![],
                        body vec![
                            mkast!(num 1.0, loc str_loc!("1 != 함수 { ", "1"))
                        ],
                    ),
                ),
            ]),
            mkerr!(BadTypeEqRightOperand, str_loc!("1 != ", "함수 { 1 }")),
        )]
        #[case::num_and_str(
            // Represents `1 != "사과"`.
            mkast!(prog loc str_loc!("", "1 != \"사과\""), vec![
                mkast!(infix InfixBangEquals, loc str_loc!("", "1 != \"사과\""),
                    left mkast!(num 1.0, loc str_loc!("", "1")),
                    right mkast!(string loc str_loc!("1 != ", "\"사과\""), vec![
                        mkstrseg!(Str, "사과", str_loc!("1 != \"", "사과")),
                    ]),
                ),
            ]),
            mkerr!(NotSameTypeInfixOperands, str_loc!("", "1 != \"사과\"")),
        )]
        #[case::num_and_bool(
            // Represents `1 != 참`.
            mkast!(prog loc str_loc!("", "1 != 참"), vec![
                mkast!(infix InfixBangEquals, loc str_loc!("", "1 != 참"),
                    left mkast!(num 1.0, loc str_loc!("", "1")),
                    right mkast!(boolean true, loc str_loc!("1 != ", "참")),
                ),
            ]),
            mkerr!(NotSameTypeInfixOperands, str_loc!("", "1 != 참")),
        )]
        fn wrong_type_bang_equals(#[case] ast: Box<Ast>, #[case] error: EvalError) {
            assert_eval_fail!(&ast, error);
        }

        #[rstest]
        #[case::left_greater_than_right(
            // Represents `2 < 1`.
            mkast!(prog loc str_loc!("", "2 < 1"), vec![
                mkast!(infix InfixLBracket, loc str_loc!("", "2 < 1"),
                    left mkast!(num 2.0, loc str_loc!("", "2")),
                    right mkast!(num 1.0, loc str_loc!("2 < ", "1")),
                ),
            ]),
            mkval!(ValueKind::Bool(false), str_loc!("", "2 < 1"))
        )]
        #[case::left_equal_to_right(
            // Represents `1 < 1`.
            mkast!(prog loc str_loc!("", "1 < 1"), vec![
                mkast!(infix InfixLBracket, loc str_loc!("", "1 < 1"),
                    left mkast!(num 1.0, loc str_loc!("", "1")),
                    right mkast!(num 1.0, loc str_loc!("1 < ", "1")),
                ),
            ]),
            mkval!(ValueKind::Bool(false), str_loc!("", "1 < 1"))
        )]
        #[case::left_less_than_right(
            // Represents `1 < 2`.
            mkast!(prog loc str_loc!("", "1 < 2"), vec![
                mkast!(infix InfixLBracket, loc str_loc!("", "1 < 2"),
                    left mkast!(num 1.0, loc str_loc!("", "1")),
                    right mkast!(num 2.0, loc str_loc!("1 < ", "2")),
                ),
            ]),
            mkval!(ValueKind::Bool(true), str_loc!("", "1 < 2"))
        )]
        fn lbracket(#[case] ast: Box<Ast>, #[case] expected: Value) {
            assert_eval!(&ast, expected);
        }

        #[rstest]
        #[case::str_and_num(
            // Represents `"사과" < 1`.
            mkast!(prog loc str_loc!("", "\"사과\" < 1"), vec![
                mkast!(infix InfixLBracket, loc str_loc!("", "\"사과\" < 1"),
                    left mkast!(string loc str_loc!("", "\"사과\""), vec![
                        mkstrseg!(Str, "사과", str_loc!("\"", "사과")),
                    ]),
                    right mkast!(num 1.0, loc str_loc!("\"사과\" < ", "1")),
                ),
            ]),
            mkerr!(BadTypeOrdLeftOperand, str_loc!("", "\"사과\"")),
        )]
        #[case::num_and_str(
            // Represents `1 < "사과"`.
            mkast!(prog loc str_loc!("", "1 < \"사과\""), vec![
                mkast!(infix InfixLBracket, loc str_loc!("", "1 < \"사과\""),
                    left mkast!(num 1.0, loc str_loc!("", "1")),
                    right mkast!(string loc str_loc!("1 < ", "\"사과\""), vec![
                        mkstrseg!(Str, "사과", str_loc!("1 < \"", "사과")),
                    ]),
                ),
            ]),
            mkerr!(BadTypeOrdRightOperand, str_loc!("1 < ", "\"사과\"")),
        )]
        #[case::bool_and_num(
            // Represents `참 < 1`.
            mkast!(prog loc str_loc!("", "참 < 1"), vec![
                mkast!(infix InfixLBracket, loc str_loc!("", "참 < 1"),
                    left mkast!(boolean true, loc str_loc!("", "참")),
                    right mkast!(num 1.0, loc str_loc!("참 < ", "1")),
                ),
            ]),
            mkerr!(BadTypeOrdLeftOperand, str_loc!("", "참")),
        )]
        #[case::num_and_bool(
            // Represents `1 < 참`.
            mkast!(prog loc str_loc!("", "1 < 참"), vec![
                mkast!(infix InfixLBracket, loc str_loc!("", "1 < 참"),
                    left mkast!(num 1.0, loc str_loc!("", "1")),
                    right mkast!(boolean true, loc str_loc!("1 < ", "참")),
                ),
            ]),
            mkerr!(BadTypeOrdRightOperand, str_loc!("1 < ", "참")),
        )]
        #[case::closure_and_num(
            // Represents `함수 { 1 } < 1`.
            mkast!(prog loc str_loc!("", "함수 { 1 } < 1"), vec![
                mkast!(infix InfixLBracket, loc str_loc!("", "함수 { 1 } < 1"),
                    left mkast!(closure loc str_loc!("", "함수 { 1 }"),
                        params vec![],
                        body vec![
                            mkast!(num 1.0, loc str_loc!("함수 { ", "1"))
                        ],
                    ),
                    right mkast!(num 1.0, loc str_loc!("함수 { 1 } < ", "1")),
                ),
            ]),
            mkerr!(BadTypeOrdLeftOperand, str_loc!("", "함수 { 1 }")),
        )]
        #[case::num_and_closure(
            // Represents `1 < 함수 { 1 }`.
            mkast!(prog loc str_loc!("", "1 < 함수 { 1 }"), vec![
                mkast!(infix InfixLBracket, loc str_loc!("", "1 < 함수 { 1 }"),
                    left mkast!(num 1.0, loc str_loc!("", "1")),
                    right mkast!(closure loc str_loc!("1 < ", "함수 { 1 }"),
                        params vec![],
                        body vec![
                            mkast!(num 1.0, loc str_loc!("1 < 함수 { ", "1"))
                        ],
                    ),
                ),
            ]),
            mkerr!(BadTypeOrdRightOperand, str_loc!("1 < ", "함수 { 1 }")),
        )]
        fn wrong_type_lbracket(#[case] ast: Box<Ast>, #[case] error: EvalError) {
            assert_eval_fail!(&ast, error);
        }

        #[rstest]
        #[case::left_greater_than_right(
            // Represents `2 > 1`.
            mkast!(prog loc str_loc!("", "2 > 1"), vec![
                mkast!(infix InfixRBracket, loc str_loc!("", "2 > 1"),
                    left mkast!(num 2.0, loc str_loc!("", "2")),
                    right mkast!(num 1.0, loc str_loc!("2 > ", "1")),
                ),
            ]),
            mkval!(ValueKind::Bool(true), str_loc!("", "2 > 1"))
        )]
        #[case::left_equal_to_right(
            // Represents `1 > 1`.
            mkast!(prog loc str_loc!("", "1 > 1"), vec![
                mkast!(infix InfixRBracket, loc str_loc!("", "1 > 1"),
                    left mkast!(num 1.0, loc str_loc!("", "1")),
                    right mkast!(num 1.0, loc str_loc!("1 > ", "1")),
                ),
            ]),
            mkval!(ValueKind::Bool(false), str_loc!("", "1 > 1"))
        )]
        #[case::left_less_than_right(
            // Represents `1 > 2`.
            mkast!(prog loc str_loc!("", "1 > 2"), vec![
                mkast!(infix InfixRBracket, loc str_loc!("", "1 > 2"),
                    left mkast!(num 1.0, loc str_loc!("", "1")),
                    right mkast!(num 2.0, loc str_loc!("1 > ", "2")),
                ),
            ]),
            mkval!(ValueKind::Bool(false), str_loc!("", "1 > 2"))
        )]
        fn rbracket(#[case] ast: Box<Ast>, #[case] expected: Value) {
            assert_eval!(&ast, expected);
        }

        #[rstest]
        #[case::str_and_num(
            // Represents `"사과" > 1`.
            mkast!(prog loc str_loc!("", "\"사과\" > 1"), vec![
                mkast!(infix InfixRBracket, loc str_loc!("", "\"사과\" > 1"),
                    left mkast!(string loc str_loc!("", "\"사과\""), vec![
                        mkstrseg!(Str, "사과", str_loc!("\"", "사과")),
                    ]),
                    right mkast!(num 1.0, loc str_loc!("\"사과\" > ", "1")),
                ),
            ]),
            mkerr!(BadTypeOrdLeftOperand, str_loc!("", "\"사과\"")),
        )]
        #[case::num_and_str(
            // Represents `1 > "사과"`.
            mkast!(prog loc str_loc!("", "1 > \"사과\""), vec![
                mkast!(infix InfixRBracket, loc str_loc!("", "1 > \"사과\""),
                    left mkast!(num 1.0, loc str_loc!("", "1")),
                    right mkast!(string loc str_loc!("1 > ", "\"사과\""), vec![
                        mkstrseg!(Str, "사과", str_loc!("1 > \"", "사과")),
                    ]),
                ),
            ]),
            mkerr!(BadTypeOrdRightOperand, str_loc!("1 > ", "\"사과\"")),
        )]
        #[case::bool_and_num(
            // Represents `참 > 1`.
            mkast!(prog loc str_loc!("", "참 > 1"), vec![
                mkast!(infix InfixRBracket, loc str_loc!("", "참 > 1"),
                    left mkast!(boolean true, loc str_loc!("", "참")),
                    right mkast!(num 1.0, loc str_loc!("참 > ", "1")),
                ),
            ]),
            mkerr!(BadTypeOrdLeftOperand, str_loc!("", "참")),
        )]
        #[case::num_and_bool(
            // Represents `1 > 참`.
            mkast!(prog loc str_loc!("", "1 > 참"), vec![
                mkast!(infix InfixRBracket, loc str_loc!("", "1 > 참"),
                    left mkast!(num 1.0, loc str_loc!("", "1")),
                    right mkast!(boolean true, loc str_loc!("1 > ", "참")),
                ),
            ]),
            mkerr!(BadTypeOrdRightOperand, str_loc!("1 > ", "참")),
        )]
        #[case::closure_and_num(
            // Represents `함수 { 1 } > 1`.
            mkast!(prog loc str_loc!("", "함수 { 1 } > 1"), vec![
                mkast!(infix InfixRBracket, loc str_loc!("", "함수 { 1 } > 1"),
                    left mkast!(closure loc str_loc!("", "함수 { 1 }"),
                        params vec![],
                        body vec![
                            mkast!(num 1.0, loc str_loc!("함수 { ", "1"))
                        ],
                    ),
                    right mkast!(num 1.0, loc str_loc!("함수 { 1 } > ", "1")),
                ),
            ]),
            mkerr!(BadTypeOrdLeftOperand, str_loc!("", "함수 { 1 }")),
        )]
        #[case::num_and_closure(
            // Represents `1 > 함수 { 1 }`.
            mkast!(prog loc str_loc!("", "1 > 함수 { 1 }"), vec![
                mkast!(infix InfixRBracket, loc str_loc!("", "1 > 함수 { 1 }"),
                    left mkast!(num 1.0, loc str_loc!("", "1")),
                    right mkast!(closure loc str_loc!("1 > ", "함수 { 1 }"),
                        params vec![],
                        body vec![
                            mkast!(num 1.0, loc str_loc!("1 > 함수 { ", "1"))
                        ],
                    ),
                ),
            ]),
            mkerr!(BadTypeOrdRightOperand, str_loc!("1 > ", "함수 { 1 }")),
        )]
        fn wrong_type_rbracket(#[case] ast: Box<Ast>, #[case] error: EvalError) {
            assert_eval_fail!(&ast, error);
        }
    }

    mod combinator_infix_with_env {
        use super::*;

        #[rstest]
        #[case::id_plus_num(
            // Represents `사과 + 4`.
            mkast!(prog loc str_loc!("", "사과 + 4"), vec![
                mkast!(infix InfixPlus, loc str_loc!("", "사과 + 4"),
                    left mkast!(identifier "사과", loc str_loc!("", "사과")),
                    right mkast!(num 4.0, loc str_loc!("사과 + ", "4")),
                ),
            ]),
            // Represents a binding for `사과` to `6.0`.
            root_env("사과", &mkval!(ValueKind::Number(6.0), range())),
            mkval!(ValueKind::Number(10.0), str_loc!("", "사과 + 4"))
        )]
        #[case::num_plus_id(
            // Represents `6 + 사과`.
            mkast!(prog loc str_loc!("", "6 + 사과"), vec![
                mkast!(infix InfixPlus, loc str_loc!("", "6 + 사과"),
                    left mkast!(num 6.0, loc str_loc!("", "6")),
                    right mkast!(identifier "사과", loc str_loc!("6 + ", "사과")),
                ),
            ]),
            // Represents a binding for `사과` to `4.0`.
            root_env("사과", &mkval!(ValueKind::Number(4.0), range())),
            mkval!(ValueKind::Number(10.0), str_loc!("", "6 + 사과"))
        )]
        #[case::id_minus_num(
            // Represents `사과 - 4`.
            mkast!(prog loc str_loc!("", "사과 - 4"), vec![
                mkast!(infix InfixMinus, loc str_loc!("", "사과 - 4"),
                    left mkast!(identifier "사과", loc str_loc!("", "사과")),
                    right mkast!(num 4.0, loc str_loc!("사과 - ", "4")),
                ),
            ]),
            // Represents a binding for `사과` to `6.0`.
            root_env("사과", &mkval!(ValueKind::Number(6.0), range())),
            mkval!(ValueKind::Number(2.0), str_loc!("", "사과 - 4"))
        )]
        #[case::num_minus_id(
            // Represents `6 - 사과`.
            mkast!(prog loc str_loc!("", "6 - 사과"), vec![
                mkast!(infix InfixMinus, loc str_loc!("", "6 - 사과"),
                    left mkast!(num 6.0, loc str_loc!("", "6")),
                    right mkast!(identifier "사과", loc str_loc!("6 - ", "사과")),
                ),
            ]),
            // Represents a binding for `사과` to `4.0`.
            root_env("사과", &mkval!(ValueKind::Number(4.0), range())),
            mkval!(ValueKind::Number(2.0), str_loc!("", "6 - 사과"))
        )]
        #[case::id_asterisk_num(
            // Represents `사과 * 4`.
            mkast!(prog loc str_loc!("", "사과 * 4"), vec![
                mkast!(infix InfixAsterisk, loc str_loc!("", "사과 * 4"),
                    left mkast!(identifier "사과", loc str_loc!("", "사과")),
                    right mkast!(num 4.0, loc str_loc!("사과 * ", "4")),
                ),
            ]),
            // Represents a binding for `사과` to `6.0`.
            root_env("사과", &mkval!(ValueKind::Number(6.0), range())),
            mkval!(ValueKind::Number(24.0), str_loc!("", "사과 * 4"))
        )]
        #[case::num_asterisk_id(
            // Represents `6 * 사과`.
            mkast!(prog loc str_loc!("", "6 * 사과"), vec![
                mkast!(infix InfixAsterisk, loc str_loc!("", "6 * 사과"),
                    left mkast!(num 6.0, loc str_loc!("", "6")),
                    right mkast!(identifier "사과", loc str_loc!("6 * ", "사과")),
                ),
            ]),
            // Represents a binding for `사과` to `4.0`.
            root_env("사과", &mkval!(ValueKind::Number(4.0), range())),
            mkval!(ValueKind::Number(24.0), str_loc!("", "6 * 사과"))
        )]
        #[case::id_slash_num(
            // Represents `사과 / 4`.
            mkast!(prog loc str_loc!("", "사과 / 4"), vec![
                mkast!(infix InfixSlash, loc str_loc!("", "사과 / 4"),
                    left mkast!(identifier "사과", loc str_loc!("", "사과")),
                    right mkast!(num 4.0, loc str_loc!("사과 / ", "4")),
                ),
            ]),
            // Represents a binding for `사과` to `6.0`.
            root_env("사과", &mkval!(ValueKind::Number(6.0), range())),
            mkval!(ValueKind::Number(1.5), str_loc!("", "사과 / 4"))
        )]
        #[case::num_slash_id(
            // Represents `6 / 사과`.
            mkast!(prog loc str_loc!("", "6 / 사과"), vec![
                mkast!(infix InfixSlash, loc str_loc!("", "6 / 사과"),
                    left mkast!(num 6.0, loc str_loc!("", "6")),
                    right mkast!(identifier "사과", loc str_loc!("6 / ", "사과")),
                ),
            ]),
            // Represents a binding for `사과` to `4.0`.
            root_env("사과", &mkval!(ValueKind::Number(4.0), range())),
            mkval!(ValueKind::Number(1.5), str_loc!("", "6 / 사과"))
        )]
        #[case::id_percent_num(
            // Represents `사과 % 4`.
            mkast!(prog loc str_loc!("", "사과 % 4"), vec![
                mkast!(infix InfixPercent, loc str_loc!("", "사과 % 4"),
                    left mkast!(identifier "사과", loc str_loc!("", "사과")),
                    right mkast!(num 4.0, loc str_loc!("사과 % ", "4")),
                ),
            ]),
            // Represents a binding for `사과` to `6.0`.
            root_env("사과", &mkval!(ValueKind::Number(6.0), range())),
            mkval!(ValueKind::Number(2.0), str_loc!("", "사과 % 4"))
        )]
        #[case::num_percent_id(
            // Represents `6 % 사과`.
            mkast!(prog loc str_loc!("", "6 % 사과"), vec![
                mkast!(infix InfixPercent, loc str_loc!("", "6 % 사과"),
                    left mkast!(num 6.0, loc str_loc!("", "6")),
                    right mkast!(identifier "사과", loc str_loc!("6 % ", "사과")),
                ),
            ]),
            // Represents a binding for `사과` to `4.0`.
            root_env("사과", &mkval!(ValueKind::Number(4.0), range())),
            mkval!(ValueKind::Number(2.0), str_loc!("", "6 % 사과"))
        )]
        #[case::id_conjunct_bool(
            // Represents `사과 그리고 참`.
            mkast!(prog loc str_loc!("", "사과 그리고 참"), vec![
                mkast!(infix InfixConjunct, loc str_loc!("", "사과 그리고 참"),
                    left mkast!(identifier "사과", loc str_loc!("", "사과")),
                    right mkast!(boolean true, loc str_loc!("사과 그리고 ", "참")),
                ),
            ]),
            // Represents a binding for `사과` to `참`.
            root_env("사과", &mkval!(ValueKind::Bool(true), range())),
            mkval!(ValueKind::Bool(true), str_loc!("", "사과 그리고 참"))
        )]
        #[case::bool_conjunct_id(
            // Represents `참 그리고 사과`.
            mkast!(prog loc str_loc!("", "참 그리고 사과"), vec![
                mkast!(infix InfixConjunct, loc str_loc!("", "참 그리고 사과"),
                    left mkast!(boolean true, loc str_loc!("", "참")),
                    right mkast!(identifier "사과", loc str_loc!("참 그리고 ", "사과")),
                ),
            ]),
            // Represents a binding for `사과` to `참`.
            root_env("사과", &mkval!(ValueKind::Bool(true), range())),
            mkval!(ValueKind::Bool(true), str_loc!("", "참 그리고 사과"))
        )]
        #[case::id_disjunct_bool(
            // Represents `사과 그리고 거짓`.
            mkast!(prog loc str_loc!("", "사과 그리고 거짓"), vec![
                mkast!(infix InfixDisjunct, loc str_loc!("", "사과 그리고 거짓"),
                    left mkast!(identifier "사과", loc str_loc!("", "사과")),
                    right mkast!(boolean false, loc str_loc!("사과 그리고 거", "짓")),
                ),
            ]),
            // Represents a binding for `사과` to `거짓`.
            root_env("사과", &mkval!(ValueKind::Bool(false), range())),
            mkval!(ValueKind::Bool(false), str_loc!("", "사과 그리고 거짓"))
        )]
        #[case::bool_disjunct_id(
            // Represents `거짓 그리고 사과`.
            mkast!(prog loc str_loc!("", "거짓 그리고 사과"), vec![
                mkast!(infix InfixDisjunct, loc str_loc!("", "거짓 그리고 사과"),
                    left mkast!(boolean false, loc str_loc!("", "거짓")),
                    right mkast!(identifier "사과", loc str_loc!("거짓 그리고 ", "사과")),
                ),
            ]),
            // Represents a binding for `사과` to `거짓`.
            root_env("사과", &mkval!(ValueKind::Bool(false), range())),
            mkval!(ValueKind::Bool(false), str_loc!("", "거짓 그리고 사과"))
        )]
        fn expression(#[case] ast: Box<Ast>, #[case] mut env: Env, #[case] expected: Value) {
            assert_eval!(&ast, &mut env, expected);
        }
    }

    #[rstest]
    #[case::evaluated_as_conseq(
        // Represents `만약 참 { 1 } 아니면 { 2 }`.
        mkast!(prog loc str_loc!("", "만약 참 { 1 } 아니면 { 2 }"), vec![
            mkast!(branch loc str_loc!("", "만약 참 { 1 } 아니면 { 2 }"),
                pred mkast!(boolean true, loc str_loc!("만약 ", "참")),
                conseq vec![
                    mkast!(num 1.0, loc str_loc!("만약 참 { ", "1")),
                ],
                altern vec![
                    mkast!(num 2.0, loc str_loc!("만약 참 { 1 } 아니면 { ", "2")),
                ],
            )
        ]),
        mkval!(ValueKind::Number(1.0), str_loc!("", "만약 참 { 1 } 아니면 { 2 }"))
    )]
    #[case::evaluated_as_altern(
        // Represents `만약 거짓 { 1 } 아니면 { 2 }`.
        mkast!(prog loc str_loc!("", "만약 거짓 { 1 } 아니면 { 2 }"), vec![
            mkast!(branch loc str_loc!("", "만약 거짓 { 1 } 아니면 { 2 }"),
                pred mkast!(boolean false, loc str_loc!("만약 ", "거짓")),
                conseq vec![
                    mkast!(num 1.0, loc str_loc!("만약 거짓 { ", "1")),
                ],
                altern vec![
                    mkast!(num 2.0, loc str_loc!("만약 거짓 { 1 } 아니면 { ", "2")),
                ],
            )
        ]),
        mkval!(ValueKind::Number(2.0), str_loc!("", "만약 거짓 { 1 } 아니면 { 2 }"))
    )]
    #[case::three_way_evaluated_as_second(
        // Represents `만약 거짓 { 1 } 아니면 만약 참 { 2 } 아니면 { 3 }`.
        mkast!(prog loc str_loc!("", "만약 거짓 { 1 } 아니면 만약 참 { 2 } 아니면 { 3 }"), vec![
            mkast!(branch loc str_loc!("", "만약 거짓 { 1 } 아니면 만약 참 { 2 } 아니면 { 3 }"),
                pred mkast!(boolean false, loc str_loc!("만약 ", "거짓")),
                conseq vec![
                    mkast!(num 1.0, loc str_loc!("만약 거짓 { ", "1")),
                ],
                altern vec![
                    mkast!(branch loc str_loc!("만약 거짓 { 1 } 아니면 ", "만약 참 { 2 } 아니면 { 3 }"),
                        pred mkast!(boolean true, loc str_loc!("만약 거짓 { 1 } 아니면 만약 ", "참")),
                        conseq vec![
                            mkast!(num 2.0, loc str_loc!("만약 거짓 { 1 } 아니면 만약 참 { ", "2")),
                        ],
                        altern vec![
                            mkast!(num 3.0, loc str_loc!("만약 거짓 { 1 } 아니면 만약 참 { 2 } 아니면 { ", "3")),
                        ],
                    ),
                ],
            )
        ]),
        mkval!(ValueKind::Number(2.0), str_loc!("", "만약 거짓 { 1 } 아니면 만약 참 { 2 } 아니면 { 3 }"))
    )]
    #[case::three_way_evaluated_as_third(
        // Represents `만약 거짓 { 1 } 아니면 만약 거짓 { 2 } 아니면 { 3 }`.
        mkast!(prog loc str_loc!("", "만약 거짓 { 1 } 아니면 만약 거짓 { 2 } 아니면 { 3 }"), vec![
            mkast!(branch loc str_loc!("", "만약 거짓 { 1 } 아니면 만약 거짓 { 2 } 아니면 { 3 }"),
                pred mkast!(boolean false, loc str_loc!("만약 ", "거짓")),
                conseq vec![
                    mkast!(num 1.0, loc str_loc!("만약 거짓 { ", "1")),
                ],
                altern vec![
                    mkast!(branch loc str_loc!("만약 거짓 { 1 } 아니면 ", "만약 거짓 { 2 } 아니면 { 3 }"),
                        pred mkast!(boolean false, loc str_loc!("만약 거짓 { 1 } 아니면 만약 ", "거짓")),
                        conseq vec![
                            mkast!(num 2.0, loc str_loc!("만약 거짓 { 1 } 아니면 만약 거짓 { ", "2")),
                        ],
                        altern vec![
                            mkast!(num 3.0, loc str_loc!("만약 거짓 { 1 } 아니면 만약 거짓 { 2 } 아니면 { ", "3")),
                        ],
                    ),
                ],
            )
        ]),
        mkval!(ValueKind::Number(3.0), str_loc!("", "만약 거짓 { 1 } 아니면 만약 거짓 { 2 } 아니면 { 3 }"))
    )]
    fn branch(#[case] ast: Box<Ast>, #[case] expected: Value) {
        assert_eval!(&ast, expected);
    }

    #[rstest]
    #[case::num_pred(
        // Represents `만약 1 { 2 } 아니면 { 3 }`.
        mkast!(prog loc str_loc!("", "만약 1 { 2 } 아니면 { 3 }"), vec![
            mkast!(branch loc str_loc!("", "만약 1 { 2 } 아니면 { 3 }"),
                pred mkast!(num 1.0, loc str_loc!("만약 ", "1")),
                conseq vec![
                    mkast!(num 2.0, loc str_loc!("만약 1 { ", "2")),
                ],
                altern vec![
                    mkast!(num 3.0, loc str_loc!("만약 1 { 2 } 아니면 { ", "3")),
                ],
            )
        ]),
        mkerr!(NonBoolPred, str_loc!("만약 ", "1")),
    )]
    #[case::str_pred(
        // Represents `만약 "사과" { 1 } 아니면 { 2 }`.
        mkast!(prog loc str_loc!("", "만약 \"사과\" { 1 } 아니면 { 2 }"), vec![
            mkast!(branch loc str_loc!("", "만약 \"사과\" { 1 } 아니면 { 2 }"),
                pred mkast!(string loc str_loc!("만약 ", "\"사과\""), vec![
                    mkstrseg!(Str, "사과", str_loc!("만약 \"", "사과")),
                ]),
                conseq vec![
                    mkast!(num 1.0, loc str_loc!("만약 \"사과\" { ", "1")),
                ],
                altern vec![
                    mkast!(num 2.0, loc str_loc!("만약 \"사과\" { 1 } 아니면 { ", "2")),
                ],
            )
        ]),
        mkerr!(NonBoolPred, str_loc!("만약 ", "\"사과\"")),
    )]
    fn invalid_pred(#[case] ast: Box<Ast>, #[case] error: EvalError) {
        assert_eval_fail!(&ast, error);
    }

    #[rstest]
    #[case::two_numbers(
        // Represents `1 2`.
        mkast!(prog loc str_loc!("", "1 2"), vec![
            mkast!(num 1.0, loc str_loc!("", "1")),
            mkast!(num 2.0, loc str_loc!("1 ", "2")),
        ]),
        // Expect the evaluated result of a multiple-expression program to be the value of the last expression.
        mkval!(ValueKind::Number(2.0), str_loc!("", "1 2"))
    )]
    #[case::assignment_and_identifier(
        // Represents `사과 = 1 사과`.
        mkast!(prog loc str_loc!("", "사과 = 1 사과"), vec![
            mkast!(infix InfixEquals, loc str_loc!("", "사과 = 1 사과"),
                left mkast!(identifier "사과", loc str_loc!("", "사과")),
                right mkast!(num 1.0, loc str_loc!("사과 = ", "1")),
            ),
            mkast!(identifier "사과", loc str_loc!("사과 = 1 ", "사과")),
        ]),
        mkval!(ValueKind::Number(1.0), str_loc!("", "사과 = 1 사과"))
    )]
    fn multiple_expressions_program(#[case] ast: Box<Ast>, #[case] expected: Value) {
        assert_eval!(&ast, expected);
    }

    mod fixtures {
        use super::*;

        pub fn root_empty_env() -> Env {
            Env::new()
        }

        pub fn root_env(id_name: &str, id_value: &Value) -> Env {
            let mut env = Env::new();
            env.set(id_name, id_value);
            env
        }

        pub fn range() -> Range {
            Range::ORIGIN
        }
    }
}
