import React from "react";
import { mount } from "enzyme";
import { expect } from "chai";
import Header from "../src/components/Header";

describe("<Header />", () => {
  it("has home link", () => {
    const wrapper = mount(<Header />);
    expect(wrapper.find(".logo")).to.exist;
  });
  it("has github link", () => {
    const wrapper = mount(<Header />);
    expect(wrapper.find(".btn")).to.exist;
  });
});
