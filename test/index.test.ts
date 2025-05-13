import init, { execute } from "../dist/komi.js";
import { describe, it, beforeAll, expect } from "bun:test";

beforeAll(async () => {
  await init();
});

describe("ok", () => {
  it("single number literal", async () => {
    const executed = execute("12.25");

    expect(executed).toBe("komi v1 ok 12.25");
  });

  it("addition", async () => {
    const executed = execute("12 + 34.675");

    expect(executed).toBe("komi v1 ok 46.675");
  });
});

describe("err", () => {
  it("err", async () => {
    const executed = execute("^");

    expect(executed.startsWith("komi v1 err")).toBe(true);
  });
});