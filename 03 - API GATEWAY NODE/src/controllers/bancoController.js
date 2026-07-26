const crypto = require('crypto');
const { simularCoelsa } = require('../services/coelsaService');
const { simularArca } = require('../services/arcaService');
const { ejecutarProgramaCobol } = require('../gateways/cobolGateway');

const login = async (req, res) => {
  const { usuario, password } = req.body;
  if (!usuario || !password) return res.status(400).json({ error: "Usuario y contraseña requeridos" });

  const lkInputStr = usuario.toUpperCase().padEnd(8, ' ') + password.padEnd(8, ' ');

  try {
    const lkOutputStr = await ejecutarProgramaCobol('PBNKLAPI', { name: 'LK_INPUT', type: '16A', value: lkInputStr }, 42);
    const codigoRespuesta = lkOutputStr.substring(0, 2).trim();
    const mensajeRespuesta = lkOutputStr.substring(2).trim();

    if (codigoRespuesta === 'OK') {
      return res.json({ status: "SUCCESS", message: mensajeRespuesta });
    } else {
      return res.status(401).json({ status: "UNAUTHORIZED", message: mensajeRespuesta });
    }
  } catch (err) {
    return res.status(500).json({ error: err.message });
  }
};

const transaccionCaja = async (req, res) => {
  const { usuario, operacion, monto } = req.body;
  if (!usuario || !operacion) return res.status(400).json({ error: "Datos insuficientes" });

  const montoFormateado = (parseFloat(monto || 0) * 100).toFixed(0).padStart(10, '0');
  const lkInputStr = usuario.toUpperCase().padEnd(8, ' ') + operacion.substring(0, 1).toUpperCase() + montoFormateado;

  try {
    const lkOutputStr = await ejecutarProgramaCobol('PBNKXAPI', { name: 'LK_INPUT', type: '19A', value: lkInputStr }, 52);
    const codigoRespuesta = lkOutputStr.substring(0, 2).trim();
    const saldoWeb = (parseInt(lkOutputStr.slice(-10), 10) / 100).toFixed(2);
    const mensajeRespuesta = lkOutputStr.substring(2, lkOutputStr.length - 10).trim();

    if (codigoRespuesta === 'OK') {
      return res.json({ status: "SUCCESS", message: mensajeRespuesta, saldo: saldoWeb });
    } else {
      return res.status(400).json({ status: "ERROR", message: mensajeRespuesta });
    }
  } catch (err) {
    return res.status(500).json({ error: err.message });
  }
};

const transferenciaBimodal = async (req, res) => {
  const { usuarioOrigen, usuarioDestino, monto } = req.body;
  if (!usuarioOrigen || !usuarioDestino || !monto) return res.status(400).json({ message: "Parámetros incompletos" });

  const idIdempotencia = req.headers['x-idempotency-key'] || crypto.randomUUID();
  const idOperacion = crypto.randomUUID(); 
  
  let idCoelsa = '', caeAfip = '', comision = 0.00, impuesto = 0.00;
  const tipoRed = usuarioDestino.length === 22 ? 'E' : 'I';

  if (tipoRed === 'E') {
    try {
      idCoelsa = await simularCoelsa(usuarioDestino);
      comision = 15.00;
      impuesto = comision * 0.21; 
      caeAfip = await simularArca(comision);
    } catch (apiError) {
      return res.status(503).json({ message: apiError.message });
    }
  }

  const formatMonto = (val) => Math.round(val * 100).toString().padStart(13, '0');
  const lkInputStr = idOperacion.padEnd(36, ' ') + idIdempotencia.padEnd(36, ' ') + 
                     idCoelsa.padEnd(22, ' ') + caeAfip.padEnd(14, ' ') + 
                     usuarioOrigen.toUpperCase().padEnd(8, ' ') + usuarioDestino.toUpperCase().padEnd(22, ' ') + 
                     tipoRed + formatMonto(parseFloat(monto)) + formatMonto(comision) + formatMonto(impuesto);

  try {
    const lkOutputStr = await ejecutarProgramaCobol('PBNKTAPI', { name: 'LK_INPUT', type: '178A', value: lkInputStr }, 55);
    const codigoRespuesta = lkOutputStr.substring(0, 2).trim();
    const saldoWeb = (parseInt(lkOutputStr.slice(-13), 10) / 100).toFixed(2);
    const mensajeRespuesta = lkOutputStr.substring(2, lkOutputStr.length - 13).trim();

    if (codigoRespuesta === 'OK') {
      return res.json({ 
        status: "SUCCESS", message: mensajeRespuesta, saldo_restante: saldoWeb,
        auditoria: { id_operacion: idOperacion, id_coelsa: idCoelsa || "Interna P2P", factura_afip: caeAfip || "Exenta" },
        desglose: { neto_transferido: parseFloat(monto).toFixed(2), comision_fintech: comision.toFixed(2), iva_recaudado: impuesto.toFixed(2) }
      });
    } else {
      return res.status(400).json({ status: "RECHAZADA", message: mensajeRespuesta });
    }
  } catch (err) {
    return res.status(500).json({ error: err.message });
  }
};

const historialMovimientos = async (req, res) => {
  const { usuario, fechaDesde, fechaHasta, pagina } = req.body;
  if (!usuario) return res.status(400).json({ error: "Usuario requerido" });

  const offsetFixed = ((parseInt(pagina) || 1) - 1) * 10;
  const lkInputStr = usuario.toUpperCase().padEnd(8, ' ') + (fechaDesde || '0001-01-01').padEnd(10, ' ') + 
                     (fechaHasta || '9999-12-31').padEnd(10, ' ') + offsetFixed.toString().padStart(4, '0');

  try {
    const lkOutputStr = await ejecutarProgramaCobol('PBNKHAPI', { name: 'LK_INPUT', type: '32A', value: lkInputStr }, 334);
    const codigoRespuesta = lkOutputStr.substring(0, 2).trim();
    const count = parseInt(lkOutputStr.substring(42, 44), 10) || 0;
    
    const movimientos = [];
    let offsetXML = 44;
    for (let i = 0; i < count; i++) {
      movimientos.push({
        fecha: lkOutputStr.substring(offsetXML, offsetXML + 10).trim(),
        tipo: lkOutputStr.substring(offsetXML + 10, offsetXML + 11).trim(),
        monto: (parseInt(lkOutputStr.substring(offsetXML + 11, offsetXML + 21).trim(), 10) / 100).toFixed(2),
        rel: lkOutputStr.substring(offsetXML + 21, offsetXML + 29).trim()
      });
      offsetXML += 29; 
    }

    if (codigoRespuesta === 'OK') return res.json({ status: "SUCCESS", movimientos });
    else return res.status(400).json({ status: "ERROR" });
  } catch (err) {
    return res.status(500).json({ error: err.message });
  }
};

module.exports = { login, transaccionCaja, transferenciaBimodal, historialMovimientos };