const redisClient = require('../config/redis');

const validarIdempotencia = async (req, res, next) => {
    const idempotencyKey = req.headers['x-idempotency-key'];

    if (!idempotencyKey) {
        return res.status(400).json({ status: "ERROR", message: "Falta la llave de idempotencia." });
    }

    try {
        const fueGuardado = await redisClient.set(idempotencyKey, 'PROCESANDO', 'EX', 86400, 'NX');

        if (fueGuardado === 'OK') {
            // DISPLAY 1: Petición original, pasa al Mainframe
            console.log(`\n[ESCUDO REDIS] 🟢 Llave NUEVA. Dejando pasar hacia DB2... UUID: ${idempotencyKey}`);
            next();
        } else {
            // DISPLAY 2: Petición repetida, rebota en memoria
            console.log(`[ESCUDO REDIS] 🔴 ¡ALERTA! Transacción duplicada bloqueada. UUID: ${idempotencyKey}`);
            return res.status(409).json({ 
                status: "ERROR", 
                message: "Esta operación ya fue recibida y está en proceso. Se bloqueó la duplicidad." 
            });
        }
    } catch (err) {
        console.error("Fallo en la validación de Redis", err);
        next(); 
    }
};

module.exports = validarIdempotencia;