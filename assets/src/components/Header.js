import React, { Component } from "react";
import { library } from "@fortawesome/fontawesome-svg-core";
import { FontAwesomeIcon } from "@fortawesome/react-fontawesome";
import { faGithub } from "@fortawesome/free-brands-svg-icons";

import "../styles/header.scss";
export default class Header extends Component {
  render() {
    return (
      <header className="header d-flex justify-content-between align-items-center">
        <a href="/" className="logo">
          Orcasound
        </a>
        <a
          href="https://github.com/orcasound"
          target="_blank"
          className="btn btn-outline-primary"
        >
          <FontAwesomeIcon icon={faGithub} className="mr-2" />
          Github
        </a>
      </header>
    );
  }
}
