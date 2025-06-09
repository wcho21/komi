# Komi

<img src="./doc/image/logo.png" alt="Kal logo" width="192px" height="192px" />

A Korean programming language based on WebAssembly.

## Features

Komi is a small interpreter that

- Runs natively in web browsers, since it's compiled to WebAssembly.
- Written in a memory-safe way using Rust.
- Does not include features with side effects such as file system or network I/O, which helps prevent harmful code injection from causing damaging in applications.

## Examples

### Hello World

You can write a string using the built-in `쓰기()` function.

```
쓰기("안녕!👋")
```

```
안녕!👋
```

As you can see, Komi natively supports Hangul in keywords and all Unicode characters in strings, because it internally processes characters in UTF-8.

If this example feels a bit boring, let's greet someone by name.
You can assign a value to a variable and use string interpolation with braces `{}`.

```
이름 = "코미"
쓰기("{이름}, 안녕!👋")
```

```
코미, 안녕!👋
```

### Making a Closure

If you want to greet other people, you can define a closure using the `함수` keyword.

```
인사 = 함수 이름 {
  "{이름}, 반가워😎"
}
인사("코미")
```

```
코미, 반가워😎
```

A closures is nothing but a value, so you can assign it to a variable.

Closures also capture variables from their outer scope into their execution environment (as in other programming languages).
This allows you to move the greeting message out of the closure body.

```
인사말 = "반가워😎" # 🖼️ Captured
인사 = 함수 이름 {
  "{이름}, {인사말}" # 📸 Capturing
}
인사("코미")
```

```
코미, 반가워😎
```

You can define higher-order closures and curry functions.

```
인사 = 함수 이름 {
  함수 인사말 {
    "{이름}, {인사말}!"
  }
}
인사("코미")("반가워😎")
```

```
코미, 반가워😎
```

Of course, you could also define multiple parameters using `함수 이름, 인사말` and call it with `인사("코미", "반가워😎")`.

### Everything is an Expression

Komi is an expression-based language.

Strings and closures are expressions, as you've seen.
Additionally, a sequence of expressions is also an expression.

```
평균 = 함수 숫자1, 숫자2 {
  합 = 숫자1 + 숫자2
  합 / 2
}
평균(10, 20)
```

```
15
```

When you call a closure, it executes all expressions in the body, but they are reduced to the value of the last expression.
This is why the last expression becomes the return value.

Since everything is and must be an expression, the closure can't have an empty body.

### Branching

Branching is also an expression, so both branches must contain at least a single expression.

```
온도 = 35
만약 온도 > 30 {
  "더워"
} 아니면 {
  "안 더워"
}
```

```
더워
```

You can also add conditions using "아니면 만약", similar to `else if` in other programming languages.

```
만약 온도 > 30 {
  "더워"
} 아니면 만약 온도 > 10 {
  "괜찮아"
} 아니면 {
  "추워"
}
```

### Types

Komi supports primitive types: numbers, strings, and booleans.

```
숫자 = 42
문자 = "사과"
불 = 참 # or 거짓
```

Note that there is no type coercion.
For example, you can't treat `0` as a false boolean value, as loosely-typed languages might do.

Komi isn't strongly-typed, but you can check the type of a value using the built-in `타입()` function.

```
타입(42)
```

```
숫자
```

### Method

> [!WARNING]
> This feature is unimplemented yet.

When you've made a closure, Komi automatically supports method-style call syntax.

```
최대 = 함수 이전, 새값 {
  만약 이전 > 새값 {
    이전
  } 아니면 {
    새값
  }
}
10.최대(5).최대(15)
```

```
15
```

The dot operator calls the closure with the left-hand value as the first argument, and the remaining call arguments as the rest.
In object-oriented terms, it sends a message to the value on the left.

You can chain methods using this operator.

```
10.최대(5).최대(-5).최대(15)
```

It is virtually equivalent to this call expression.

```
최대(최대(최대(10, 5), -5), 15)
```

### Iterator

> [!WARNING]
> This feature is unimplemented yet.

Komi supports iteration through iterators.

```
범위(5).줄이기(더하기) # 0부터 4까지의 합
```

```
10
```

Iterators are basically lazy-evaluated, so you can express a range from `0` to infinity.
Using this, the above example can also be written as below.

```
범위(0, ..).갖기(5).줄이기(더하기) # 0부터 4까지의 합
```

As functional programming languages provides `map()`, `filter()`, and `reduce()`, Komi supports these functions as well.

```
# 홀수의 제곱 합
범위(5)
  .고르기(함수 숫자 { 숫자 % 2 == 0 }) # 1, 3, 5
  .바꾸기(함수 숫자 { 숫자 * 숫자 }) # 1, 9, 25
  .줄이기(더하기) # 35
```

```
35
```

## JavaScript Interface

You can run the interpreter with the `execute()` function in this package.

```ts
import { execute } from "@wcho21/komi_wasm";

execute("안녕")
```

If execution succeeds, the result is the following object.

```ts
interface ExecOut {
  value: string;
  stdout: string;
}
```

where `value` is the evaluation result, and `stdout` contains the stdout.

If execution fails, a JavaScript `Error` instance is thrown.

```ts
interface ExecError {
  name: "LexError" | "ParseError" | "EvalError";
  message: string;
  cause: {
    location: {
      begin: {
        col: number;
        row: number;
      },
      end: {
        col: number;
        row: number;
      },
    };
  };
}
```

where `name` and `messsage` specifies the reason of the error, `cause` has the location in the source code where the error occured.

## Installation

Install the package with `npm install @wcho21/komi_wasm` via GitHub Packages repository.
See the [documentation][gh-pack].

[gh-pack]: https://docs.github.com/en/packages/working-with-a-github-packages-registry/working-with-the-npm-registry#installing-a-package