const express = require('express');
const cors = require('cors'); 
require('dotenv').config();

const apiRoutes = require('./src/routes/apiRoutes');

const app = express();
const puerto = process.env.PORT || 3000;

// Middlewares Globales
app.use(cors()); 
app.use(express.json());

// Enrutador Principal (Capa de Red / API Gateway)
app.use('/api', apiRoutes);

// Manejo de rutas inexistentes (404 Fallback)
app.use((req, res) => {
  res.status(404).json({ error: "Endpoint no encontrado en CoreBank API Gateway" });
});

app.listen(puerto, () => {
  console.log(`[API Gateway] Modo 2 Activo. Arquitectura Hexagonal en puerto http://localhost:${puerto}`);
});