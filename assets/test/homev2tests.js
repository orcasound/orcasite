import React from "react"
import { shallow, mount } from "enzyme"
import { expect } from "chai"
import HomeV2 from "../src/components/HomeV2"


describe("<HomeV2 />", () => {
  it("has <SiteMenu />", () => {
    const wrapper = shallow(<HomeV2 />)
    console.log(wrapper.debug())
    expect(wrapper.find("SiteMenu")).to.exist
  })

  it("has <AudioPlayerV2 />", () => {
    const wrapper = shallow(<HomeV2 />)
    expect(wrapper.find("AudioPlayerV2")).to.exist
  })

  it("has <MuiThemeProvider />", () => {
    const wrapper = shallow(<HomeV2 />)
    expect(wrapper.find("MuiThemeProviderOld")).to.exist
  })
})
