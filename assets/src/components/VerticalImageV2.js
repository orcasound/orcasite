import React from "react"
import OrcaImage from "../../static/spyhops_ship_updated.jpg"
import styled from "styled-components"

const StyledImageContainer = styled.div`
  display: grid;
  margin: 1rem 1rem 1rem 0rem;
`

const VerticalImageV2 = () => {
  return (
    <StyledImageContainer className="image-container">
      <img src={OrcaImage} />
    </StyledImageContainer>
  )
}

export default VerticalImageV2
