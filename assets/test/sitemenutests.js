import React from "react"
import { mount } from "enzyme"
import { expect } from "chai"
import SiteMenu from "../src/components/SiteMenu"

describe("<SiteMenu />", () => {
  describe("<Tabs />", () => {
    it("has 2 <Tab />", () => {
      const wrapper = mount(<SiteMenu />)
      expect(wrapper.find("Tabs")).to.exist
      expect(wrapper.find("Tab")).to.have.lengthOf(2)
    })
    it("has About tab", () => {
      const wrapper = mount(<SiteMenu />)
      expect(
        wrapper
          .find("Tab")
          .at(0)
          .prop("label")
      ).to.be.eql("About")
    })
    it("has Feeds tab", () => {
      const wrapper = mount(<SiteMenu />)
      expect(
        wrapper
          .find("Tab")
          .at(1)
          .prop("label")
      ).to.be.eql("Listen Live")
    })
  })
  describe("notifications link", () => {
    it("has expected text", () => {
      const wrapper = mount(<SiteMenu />)
      expect(wrapper.find(".sitemenu-notification-link").at(0))
        .text()
        .to.be.eql("Get notified when there's whale activity")
    })
  })
})
