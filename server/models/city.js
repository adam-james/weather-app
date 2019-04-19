const { model, Schema } = require("mongoose");

const schema = new Schema({
  id: Number,
  name: String,
  country: String,
  coord: {
    lon: Number,
    lat: Number
  }
});

module.exports = model("City", schema);
