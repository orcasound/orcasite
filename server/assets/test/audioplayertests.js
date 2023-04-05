import React from "react"
import { createShallow } from "@material-ui/core/test-utils"
import { expect } from "chai"
import "styled-components-test-utils/lib/chai"
import renderer from "react-test-renderer"
import AudioPlayer, {
  CardWrapper,
  AudioHeader,
  Audio
} from "../src/components/AudioPlayer"

describe("<AudioPlayer />", () => {
  let shallow

  before(() => {
    shallow = createShallow()
  })

  describe("has expected audio players", () => {
    let wrapper

    before(() => {
      wrapper = shallow(<AudioPlayer />)
    })

    it("has call example audio player", () => {
      const call_example_audio_player = wrapper.find("#call-examples")
      expect(call_example_audio_player).to.exist
      expect(call_example_audio_player)
        .prop("src")
        .to.be.eql(
          "http://www.orcasound.net/data/product/SRKW/orcasite/call-examples.mp3"
        )
      expect(call_example_audio_player)
        .prop("controls")
        .to.be.eql(true)
      expect(call_example_audio_player)
        .to.exist.prop("controls")
        .to.be.eql(true)
    })

    it("has four minute sample", () => {
      const four_minute_sample = wrapper.find("#four-minute-sample")
      expect(four_minute_sample).to.exist
      expect(four_minute_sample)
        .prop("src")
        .to.be.eql(
          "http://www.orcasound.net/data/product/SRKW/orcasite/4min-sample.mp3"
        )
      expect(four_minute_sample)
        .to.exist.prop("controls")
        .to.be.eql(true)
      expect(four_minute_sample)
        .to.exist.prop("controls")
        .to.be.eql(true)
    })

    it("has whistle examples", () => {
      const whistle_examples = wrapper.find("#whistle-examples")
      expect(whistle_examples).to.exist
      expect(whistle_examples)
        .prop("src")
        .to.be.eql(
          "http://www.orcasound.net/data/product/SRKW/orcasite/whistle-examples.mp3"
        )
      expect(whistle_examples)
        .to.exist.prop("controls")
        .to.be.eql(true)
    })

    describe("styles", () => {
      it("CardWrapper", () => {
        const card_wrapper = renderer.create(<CardWrapper />)
        expect(card_wrapper).toHaveStyleRule("display", "flex")
        expect(card_wrapper).toHaveStyleRule("justify-content", "center")
      })
      it("AudioHeader", () => {
        const audio_header_wrapper = renderer.create(<AudioHeader />)
        expect(audio_header_wrapper).toHaveStyleRule("margin-left", "1rem")
      })
      it("Audio", () => {
        const audio_wrapper = renderer.create(<Audio />)
        expect(audio_wrapper).toHaveStyleRule("margin", ".5rem")
      })
      it("CardWrapper Alternate", () => {
        const component_wrapper = renderer.create(<AudioPlayer />)
        const card_wrapper = component_wrapper.root.findAllByType(
          CardWrapper
        )[0]
        expect(card_wrapper.type.componentStyle.rules[0]).to.eql(
          "display:flex;justify-content:center;"
        )
      })
    })
  })
})
