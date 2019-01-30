import React from "react"
import { mount } from "enzyme"
import { expect } from "chai"
import HomeV2 from "../src/components/HomeV2"

describe("<HomeV2 />", () => {
  it("has <SiteMenu />", () => {
    const wrapper = mount(<HomeV2 />)
    expect(wrapper.find("SiteMenu")).to.exist
  })
})
