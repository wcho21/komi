import { describe, it, expect } from "vitest";
import { execute } from "komi";

describe("ok", () => {
  it("single number literal", async () => {
    const executed = execute("+12.25");

    expect(executed).toBe("komi v1 ok 12.25");
  });

  it("addition, subtraction and mod", async () => {
    const executed = execute("(12 % 5) + (34.25 - 30)");

    expect(executed).toBe("komi v1 ok 6.25");
  });

  it("subtraction and division", async () => {
    const executed = execute("-1*2 * (3/4)");

    expect(executed).toBe("komi v1 ok -1.5");
  });

  it("invalid number literal", async () => {
    const executed = execute(".25");

    expect(executed.startsWith("komi v1 err"));
  });

  it("invalid arithmetic expression", async () => {
    const executed = execute("1 +");

    expect(executed.startsWith("komi v1 err"));
  });

  it("no opening parenthesis", async () => {
    const executed = execute("1 + 2)");

    expect(executed.startsWith("komi v1 err"));
  });

  it("no closing parenthesis", async () => {
    const executed = execute("(1 + 2");

    expect(executed.startsWith("komi v1 err"));
  });
});

describe("err", () => {
  it("err", async () => {
    const executed = execute("^");

    expect(executed.startsWith("komi v1 err")).toBe(true);
  });
});