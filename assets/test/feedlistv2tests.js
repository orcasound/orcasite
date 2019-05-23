import React from "react"
import { mount, shallow } from "enzyme"
import { expect } from "chai"
import sinon from "sinon"
import FeedListV2 from "../src/components/FeedListV2"


describe("<FeedListV2 />", () => {
  describe("</Button>", () => {
    it("has expected text", () => {
      const wrapper = mount(<FeedListV2 />)
      const button = wrapper.find("Button")
      expect(button).to.exist
      expect(button).has.text("Listen Live")
    })
    it.skip("should call handleToggle() when clicked", () => {
      console.log('Proto' + FeedListV2.prototype)
      const spy = sinon.spy(FeedListV2.prototype, "handleToggle")
      const wrapper = shallow(<FeedListV2 />)
      const button = wrapper.find("Button")
      button.simulate("click")
      expect(spy.calledOnce).to.equal(true)
    })
  })
  it("has menu-list-grow", () => {
    const wrapper = mount(<FeedListV2 />)
    expect(wrapper.find("#menu-list-grow")).to.exist
  })
  it("has <MenuList />", () => {
    const wrapper = mount(<FeedListV2 />)
    expect(wrapper.find("MenuList")).to.exist
  })
  describe("<MenuList />", () => {
    it("has expected menuitem count", () => {
      const wrapper = mount(<FeedListV2 />)
      const menulist = wrapper.find("MenuList")
      expect(menulist.find("MenuItem")).to.have.length(2)
    })
  })
})
