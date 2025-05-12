import init, { execute } from "../dist/komi.js";
import { describe, it, beforeAll, expect } from "bun:test";

beforeAll(async () => {
  await init();
});

describe("ok", () => {
  it("single number literal", async () => {
    const executed = execute("12.25");

    const { ok } = JSON.parse(executed);

    expect(ok).toBe("12.25");
  });
});

describe("err", () => {
  it("err", async () => {
    const executed = execute(" ");

    const { err } = JSON.parse(executed);

    expect(err).not.toBe(undefined);
  });
});