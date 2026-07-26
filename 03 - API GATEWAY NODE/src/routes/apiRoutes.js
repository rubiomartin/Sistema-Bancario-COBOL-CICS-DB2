const express = require('express');
const router = express.Router();
const validarIdempotencia = require('../middlewares/idempotencia');
const { login, transaccionCaja, transferenciaBimodal, historialMovimientos } = require('../controllers/bancoController');

router.post('/login', login);
router.post('/transaccion', transaccionCaja);
router.post('/transferencia', validarIdempotencia, transferenciaBimodal);
router.post('/historial', historialMovimientos);

module.exports = router;