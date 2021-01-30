import React from "react";
import ReactDOM from "react-dom";
import Draft from "draft-js";
import nlp from "compromise";
import "draft-js/dist/Draft.css";

const EntityDecorator = (ctx) => {
  return {
    strategy: function (contentBlock, callback, contentState) {
      const text = contentBlock.getText();
      const matchKeys = Object.keys(ctx.text).map((k) => new RegExp(k, "ig"));
      /* Naive traversal of entity names to find them on the document */
      matchKeys.forEach((key) => {
        let matches = text.matchAll(key);
        [...matches].forEach((match) => {
          callback(match.index, match.index + match[0].length);
        });
      });
    },
    component: (props) => {
      return (
        <a
          style={{
            background: "#1d38bf",
            padding: "0.3rem",
            borderRadius: "5px",
            color: "#f8f8f8",
          }}
        >
          {props.children}
        </a>
      );
    },
  };
};

class SagaApp extends React.Component {
  constructor(props) {
    super(props);

    let decorator = new Draft.CompositeDecorator([]);
    let contentState = Draft.ContentState.createFromText(props.text || "");
    let editorState = Draft.EditorState.createWithContent(
      contentState,
      decorator
    );

    this.state = {
      nouns: ["test", "dog"],
      rawText: "",
      text: {},
      entities: {},
      editorState,
    };
  }

  addEntity(name) {
    if (name.toLowerCase() == "pugna") {
      this.setState({
        ...this.state,
        text: Object.assign(this.state.text, {
          ["Pugna"]: "https://dota2.steam.org/2020/7.25a/Hero/Pugna",
        }),
        entities: Object.assign(this.state.entities, {
          ["https://dota2.steam.org/2020/7.25a/Hero/Pugna"]: {
            "@id": "https://dota2.steam.org/2020/7.25a/Hero/Pugna",
            "https://dota2.steam.org/2020/Hero/Name": "Pugna",
          },
        }),
      });
    } else {
      this.setState({
        ...this.state,
        text: Object.assign(this.state.text, {
          [name]: "https://dota2.steam.org/2020/7.25a/Hero/" + name,
        }),
      });
    }

    const entityTagger = new Draft.CompositeDecorator([EntityDecorator(this.state)]);
    const editorState = Draft.EditorState.set(this.state.editorState, {
      decorator: entityTagger,
    });

    this.setState({
      ...this.state,
      editorState,
    });
  }

  onEditorChange(editorState) {
    let rawText = editorState.getCurrentContent().getPlainText();
    let analysis = nlp(rawText);
    let nouns =
      Object.keys(Object.fromEntries(analysis.nouns().out("array").map( x => [x, 0])));
    let verbs = analysis.verbs().out("array");

    this.setState({
      ...this.state,
      rawText,
      nouns,
      verbs,
      editorState,
    });
  }

  render() {
    let { editorState, ...rest } = this.state;
    return (
      <>
        <Draft.Editor
          editorState={editorState}
          onChange={this.onEditorChange.bind(this)}
        />
        <hr />
        <div>
          <b>are these relevant entities? (click to add)</b>
        </div>
        <br />
        <div>
          {this.state.nouns.map((n) => (
            <span
              style={{
                padding: "0.3rem",
                borderRadius: "5px",
                border: "1px solid gray",
                cursor: "pointer",
                margin: "0.1rem 0.1rem",
              }}
              onClick={function (e) {
                this.addEntity(n);
              }.bind(this)}
            >
              {n}
            </span>
          ))}
        </div>
        <br />
        <div>
          <b>context: </b>
        </div>
        <pre>{JSON.stringify(rest, null, 2)}</pre>
      </>
    );
  }
}

ReactDOM.render(<SagaApp />, document.getElementById("container"));
