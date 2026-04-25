//----------------------------------------------------------------------------//

MathJax = {
  tex: {
    inlineMath: [
      ["$", "$"],
      ["\\(", "\\)"],
    ],
  },
  svg: {
    fontCache: "global",
  },
};

//----------------------------------------------------------------------------//

const baseUrl = "file:///home/casey/dev/hsk/test0/appdata/site/";
const url = window.location.href.substring(baseUrl.length);

let sk;
let interval = setInterval(() => {
  if (sk === undefined || sk.readyState === WebSocket.CLOSED) {
    sk = new WebSocket("ws://localhost:8083");

    sk.addEventListener("open", (e) => {
      sk.send(url);
      console.log("Websocket connected!");
    });

    sk.addEventListener("message", (e) => {
      switch (e.data[0]) {
        case "\x00":
          const parts = e.data.substring(1).split("\x00");
          const page = parts[0];
          const data = parts[1];
          console.log("Mutate", page);
          if (page === url) {
            document.firstElementChild.innerHTML = data;
          }
          break;
        case "\x01":
          console.log("Refresh");
          location.reload();
          break;
        case "\x02":
          console.error("Server Error", e.data.substring(1));
          break;
        default:
          console.error("Unknown action code:", e.data.charCodeAt(0));
      }
    });
  }
}, 500);

//----------------------------------------------------------------------------//
