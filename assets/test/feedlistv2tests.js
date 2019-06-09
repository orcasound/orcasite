import React from "react"
import { mount, shallow } from "enzyme"
import { expect } from "chai"
import sinon from "sinon"
import { MockedProvider } from 'react-apollo/test-utils';
import FeedListV2 from "../src/components/FeedListV2"


import { LIST_FEEDS } from '../src/queries/feeds'

const mocks = [{
  request: {
    query: LIST_FEEDS,
  },
  result: {
    "data": { "feeds": [{ "__typename": "Feed", "id": "1", "name": "Orcasound Lab (Haro Strait)", "nodeName": "rpi_orcasound_lab", "slug": "orcasound-lab", "thumbUrl": "https://s3-us-west-2.amazonaws.com/orcasite/rpi_orcasound_lab/thumbnail.png" }, { "__typename": "Feed", "id": "2", "name": "Bush Point", "nodeName": "rpi_bush_point", "slug": "bush-point", "thumbUrl": "https://s3-us-west-2.amazonaws.com/orcasite/rpi_bush_point/thumbnail.png" }] }
  }
}];

describe.only("<FeedListV2 />", () => {
  it("should render loading state initially", () => {
    const wrapper = mount((
      <MockedProvider mocks={[]}>
        <FeedListV2 />
      </MockedProvider>
    ))

    const loadingState = wrapper.find("Loading")
    expect(loadingState).to.exist
  })
})


describe("<FeedListV2 />", () => {
  describe("</Button>", () => {
    it("has expected text", () => {
      const wrapper = mount((
        <MockedProvider mocks={mocks} addTypename={false}>
          <FeedListV2 />
        </MockedProvider>
      ))
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
    const wrapper = mount(
      <MockedProvider mocks={mocks} addTypename={false}>
        <FeedListV2 />
      </MockedProvider>
    )
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
