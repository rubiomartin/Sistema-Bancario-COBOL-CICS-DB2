const { ProgramCall } = require('itoolkit');
const conexionPub400 = require('../config/pub400');
require('dotenv').config();

const BIBLIOTECA = process.env.PUB400_USER.toUpperCase() + '1';

const ejecutarProgramaCobol = (nombrePrograma, paramInput, lengthOutput) => {
  return new Promise((resolve, reject) => {
    const pgm = new ProgramCall(nombrePrograma, { lib: BIBLIOTECA });
    pgm.addParam(paramInput);
    pgm.addParam({ name: 'LK_OUTPUT', type: `${lengthOutput}A`, io: 'out' });

    conexionPub400.add(pgm);
    conexionPub400.run((error, xmlSalida) => {
      if (error) return reject(new Error("Error de red en el túnel SSH con IBM i"));
      if (xmlSalida.includes('<error>')) return reject(new Error(`Fallo de ejecución en el programa ${nombrePrograma}`));

      try {
        const match = xmlSalida.match(/<data[^>]*name='LK_OUTPUT'[^>]*>([\s\S]*?)<\/data>/);
        const lkOutputStr = match ? match[1] : '';
        if (!lkOutputStr) return reject(new Error("Sin datos devueltos por el Mainframe"));
        resolve(lkOutputStr);
      } catch (err) {
        reject(new Error("Error al procesar la trama XML devuelta por OS/400"));
      }
    });
  });
};

module.exports = { ejecutarProgramaCobol };