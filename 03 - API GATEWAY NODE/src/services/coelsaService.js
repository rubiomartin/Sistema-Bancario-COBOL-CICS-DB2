const crypto = require('crypto');

const simularCoelsa = async (identificadorDestino) => {
  return new Promise((resolve, reject) => {
    const latencia = Math.floor(Math.random() * 500) + 300; 
    
    const entidadesSoportadas = {
      '00000031': 'Mercado Pago', '00000079': 'Ualá',
      '00000062': 'Prex',         '00000018': 'Personal Pay',
      '017': 'BBVA Francés',      '072': 'Banco Santander',
      '011': 'Banco Nación',      '322': 'Banco Industrial (BIND)'
    };

    setTimeout(() => {
      if (Math.random() > 0.98) {
        return reject(new Error("Timeout: La cámara compensadora no responde."));
      }

      const prefijoCVU = identificadorDestino.substring(0, 8);
      const prefijoCBU = identificadorDestino.substring(0, 3);
      const entidadDestino = entidadesSoportadas[prefijoCVU] || entidadesSoportadas[prefijoCBU];

      if (entidadDestino) {
        console.log(`[COELSA Sandbox] 🚀 Enrutando pago hacia: ${entidadDestino}`);
        const idCoelsa = 'COELSA-' + crypto.randomBytes(7).toString('hex').toUpperCase();
        resolve(idCoelsa);
      } else {
        console.log(`[COELSA Sandbox] ❌ Rechazo: Entidad desconocida (${identificadorDestino})`);
        reject(new Error("RECHAZO COELSA: Cuenta destino inexistente o entidad no adherida."));
      }
    }, latencia);
  });
};

module.exports = { simularCoelsa };