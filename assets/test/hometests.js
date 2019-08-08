import React from "react"
import { shallow, mount } from "enzyme"
import { expect } from "chai"
import Home from "../src/components/Home"

describe("<Home />", () => {
  it("has <SiteMenu />", () => {
    const wrapper = shallow(<Home />)
    console.log(wrapper.debug())
    expect(wrapper.find("SiteMenu")).to.exist
  })

  it("has <AudioPlayer />", () => {
    const wrapper = shallow(<Home />)
    expect(wrapper.find("AudioPlayer")).to.exist
  })

  it("has <MuiThemeProvider />", () => {
    const wrapper = shallow(<Home />)
    expect(wrapper.find("MuiThemeProviderOld")).to.exist
  })
})
