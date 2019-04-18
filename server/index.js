require("dotenv").config();

const app = require("express")();
const fetch = require("node-fetch");
const port = 5000;

const { API_KEY } = process.env;
const baseUrl = "https://api.openweathermap.org/data/2.5";
const cityId = 4463523; // Denver

function getUrl(endpoint, cityId) {
  return baseUrl + endpoint + `?id=${cityId}&APPID=${API_KEY}&units=imperial`;
}

app.get("/weather", async (req, res) => {
  const endpoint = "/weather";
  const url = getUrl(endpoint, cityId);

  try {
    const resp = await fetch(url);

    if (resp.ok) {
      const json = await resp.json();
      res.json(json);
    } else {
      throw new Error("Response is not OK.");
    }
  } catch (err) {
    console.log(err);
    res.status(500).json({ error: "An error occurred" });
  }
});

app.listen(port, () => console.log(`App listening on ${port}`));
