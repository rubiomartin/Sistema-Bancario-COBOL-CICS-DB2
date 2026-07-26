const { Connection } = require('itoolkit');
require('dotenv').config();

const conexionPub400 = new Connection({
  transport: 'ssh',
  transportOptions: {
    host: 'pub400.com',
    port: 2222,
    username: process.env.PUB400_USER,
    password: process.env.PUB400_PASS
  }
});

module.exports = conexionPub400;