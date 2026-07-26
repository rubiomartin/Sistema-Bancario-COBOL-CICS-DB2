const Redis = require('ioredis');


const redisClient = new Redis({
    host: '127.0.0.1',
    port: 6380,
    
    retryStrategy: (times) => Math.min(times * 50, 2000) 
});

redisClient.on('connect', () => console.log('🟢 Redis conectado exitosamente como caché perimetral.'));
redisClient.on('error', (err) => console.error('🔴 Error en la conexión de Redis:', err));

module.exports = redisClient;