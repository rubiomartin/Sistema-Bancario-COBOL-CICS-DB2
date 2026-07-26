import { useState, useEffect } from 'react';
import axios from 'axios';
import './App.css';

function App() {
  const [usuario, setUsuario] = useState('');
  const [password, setPassword] = useState('');
  const [mensaje, setMensaje] = useState('');
  const [isLoggedIn, setIsLoggedIn] = useState(false);

  const [saldo, setSaldo] = useState(null);
  const [monto, setMonto] = useState('');
  const [destino, setDestino] = useState('');
  const [modoActivo, setModoActivo] = useState(null); 
  const [tipoCaja, setTipoCaja] = useState('D');
  
  const [historial, setHistorial] = useState([]);
  const [fechaDesde, setFechaDesde] = useState('');
  const [fechaHasta, setFechaHasta] = useState('');
  const [pagina, setPagina] = useState(1);
  const [hayMas, setHayMas] = useState(false);

  const [comprobante, setComprobante] = useState(null);

  const [llaveIdempotencia, setLlaveIdempotencia] = useState(crypto.randomUUID());


  useEffect(() => {
    setLlaveIdempotencia(crypto.randomUUID());
  }, [destino, monto]);


  const [notificacion, setNotificacion] = useState(null);

  const mostrarNotificacion = (texto, tipo) => {
    setNotificacion({ texto, tipo });
    setTimeout(() => setNotificacion(null), 4000);
  };

  const formatMoneda = (valor) => {
    if (valor === null || valor === undefined) return '---';
    const numero = parseFloat(valor);
    if (isNaN(numero)) return '---';
    
    return new Intl.NumberFormat('es-AR', {
      style: 'decimal',
      minimumFractionDigits: 2,
      maximumFractionDigits: 2
    }).format(numero);
  };

  const handleLogin = async (e) => {
    e.preventDefault();
    try {
      const res = await axios.post('http://localhost:3000/api/login', { usuario, password });
      if (res.data.status === 'SUCCESS') {
        setIsLoggedIn(true);
        setMensaje('');
        ejecutarTransaccionSimple('C', 0);
      }
    } catch (err) {
      setMensaje('❌ ' + (err.response?.data?.message || 'Error de conexión'));
    }
  };

  const ejecutarTransaccionSimple = async (operacion, montoOperar) => {
    try {
      const res = await axios.post('http://localhost:3000/api/transaccion', {
        usuario: usuario, operacion: operacion, monto: montoOperar
      });
      if (res.data.status === 'SUCCESS') {
        setSaldo(res.data.saldo);
        setModoActivo(null);
        setMonto('');
        if (operacion !== 'C') mostrarNotificacion(res.data.message, 'exito');
      }
    } catch (err) {
      mostrarNotificacion(err.response?.data?.message || 'Error en la operación', 'error');
    }
  };


  const ejecutarTransferencia = async (e) => {
    e.preventDefault();

    try {
      const res = await axios.post('http://localhost:3000/api/transferencia', {
        usuarioOrigen: usuario, 
        usuarioDestino: destino, 
        monto: monto
      }, {
        headers: { 'x-idempotency-key': llaveIdempotencia }
      });
      
      if (res.data.status === 'SUCCESS') {
        setSaldo(res.data.saldo_restante); 
        
        setComprobante({
          destino: destino,
          montoOriginal: monto,
          auditoria: res.data.auditoria,
          desglose: res.data.desglose
        });
        setModoActivo('COMPROBANTE');
        
        mostrarNotificacion(res.data.message, 'exito');
        setLlaveIdempotencia(crypto.randomUUID());
      }
    } catch (err) {
      mostrarNotificacion(err.response?.data?.message || 'Error en la transferencia', 'error');
      
      if (err.response && (err.response.status === 400 || err.response.status === 503)) {
        setLlaveIdempotencia(crypto.randomUUID());
      }
    }
  };

  const cargarHistorial = async (pag) => {
    try {
      const res = await axios.post('http://localhost:3000/api/historial', { 
        usuario, fechaDesde, fechaHasta, pagina: pag
      });
      
      if (res.data.status === 'SUCCESS') {
        const movs = res.data.movimientos || [];
        setHistorial(movs);
        setPagina(pag);
        setHayMas(movs.length === 10); 
        setModoActivo('HIST');
      } else {
        mostrarNotificacion('El servidor no devolvió estado SUCCESS', 'error');
      }
    } catch (err) {
      mostrarNotificacion('Fallo crítico al cargar historial', 'error');
    }
  };

  const verHistorial = () => {
    setFechaDesde('');
    setFechaHasta('');
    cargarHistorial(1);
  };

  const traducirOperacion = (tipo, rel) => {
    if (tipo === 'D') return 'Depósito';
    if (tipo === 'R' || tipo === 'Z') return 'Retiro';
    if (tipo === 'T') return `Transf. a ${rel}`;
    return 'Movimiento';
  };

  const getColorMonto = (tipo, rel) => {
    const usuarioRel = rel ? String(rel).trim() : '';
    if (tipo === 'D' || (tipo === 'R' && usuarioRel !== '')) return '#4ade80';
    return '#f87171';
  };

  const handleLogout = () => {
    setIsLoggedIn(false); setPassword(''); setSaldo(null); setModoActivo(null); setComprobante(null);
  };

  const resetearVista = (vista) => {
    setModoActivo(vista);
    setComprobante(null);
    setMonto('');
    setDestino('');
  };

  if (isLoggedIn) {
    return (
      <div className="dashboard-layout">
        
        {notificacion && (
          <div className={`notification-toast toast-${notificacion.tipo}`}>
            {notificacion.tipo === 'error' && <span className="toast-icon">❌</span>}
            <span className="toast-text">{notificacion.texto}</span>
          </div>
        )}

        <aside className="sidebar">
          <div className="sidebar-brand">
            <span className="logo-icon">🏦</span>
            <h2>CoreBank</h2>
          </div>
          <nav className="sidebar-nav">
            <button className={modoActivo === null ? 'active' : ''} onClick={() => resetearVista(null)}>📊 Resumen de Cuenta</button>
            <button className={modoActivo === 'CAJA' ? 'active' : ''} onClick={() => resetearVista('CAJA')}>💵 Depósitos y Retiros</button>
            <button className={modoActivo === 'TRANSF' ? 'active' : ''} onClick={() => resetearVista('TRANSF')}>🚀 Transferencias</button>
            <button className={modoActivo === 'HIST' ? 'active' : ''} onClick={verHistorial}>📋 Historial de Movimientos</button>
          </nav>
          <button className="sidebar-logout" onClick={handleLogout}>Cerrar Sesión</button>
        </aside>

        <main className="main-content">
          <header className="topbar">
            <div className="topbar-user-info">
              <p className="greeting">Bienvenido/a,</p>
              <h3 className="username">{usuario.toUpperCase()}</h3>
            </div>
            <div className="balance-badge" onClick={() => ejecutarTransaccionSimple('C', 0)}>
              <span>Saldo Disponible: </span>
              <strong>{saldo !== null ? `$ ${formatMoneda(saldo)}` : '---'}</strong>
            </div>
          </header>

          <div className="workspace">
            {modoActivo === null && (
              <div className="welcome-panel">
                <h1 className="welcome-title">Tu banco, conectado a IBM i.</h1>
                <p className="welcome-subtitle">Selecciona una operación en el menú lateral para interactuar con tu cuenta en tiempo real.</p>
                <div className="quick-stats">
                  <div className="stat-card">
                    <h4>Estado del Sistema</h4>
                    <span className="status-badge">🟢 Online (PUB400)</span>
                  </div>
                  <div className="stat-card">
                    <h4>Última Conexión</h4>
                    <span>{new Date().toLocaleDateString()}</span>
                  </div>
                </div>
              </div>
            )}

            {modoActivo === 'CAJA' && (
              <div className="form-panel">
                <h2>Operaciones de Caja</h2>
                <form onSubmit={(e) => { e.preventDefault(); ejecutarTransaccionSimple(tipoCaja, monto); }}>
                  <div className="input-group">
                    <label>Tipo de Operación</label>
                    <select value={tipoCaja} onChange={(e) => setTipoCaja(e.target.value)}>
                      <option value="D">Depositar Efectivo</option>
                      <option value="R">Retirar Efectivo</option>
                    </select>
                  </div>
                  <div className="input-group">
                    <label>Monto de la operación</label>
                    <input type="number" step="0.01" placeholder="$ 0.00" value={monto} onChange={(e) => setMonto(e.target.value)} required />
                  </div>
                  <button type="submit" className="primary-btn">Confirmar Operación</button>
                </form>
              </div>
            )}

            {modoActivo === 'TRANSF' && (
              <div className="form-panel">
                <h2>Transferencia a Terceros</h2>
                <form onSubmit={ejecutarTransferencia}>
                  <div className="input-group">
                    <label>Destino (Alias Interno o CVU)</label>
                    <input type="text" placeholder="Ej: JPEREZ o 072000... (22 dígitos)" value={destino} onChange={(e) => setDestino(e.target.value)} required />
                  </div>
                  <div className="input-group">
                    <label>Monto a transferir</label>
                    <input type="number" step="0.01" placeholder="$ 0.00" value={monto} onChange={(e) => setMonto(e.target.value)} required />
                  </div>
                  <button type="submit" className="primary-btn">Enviar Transferencia</button>
                </form>
              </div>
            )}

            {/* NUEVA VISTA: COMPROBANTE DE TRANSFERENCIA */}
            {modoActivo === 'COMPROBANTE' && comprobante && (
              <div className="form-panel" style={{ textAlign: 'left' }}>
                <h2 style={{ color: '#4ade80', borderBottom: '1px solid #333', paddingBottom: '10px' }}>✅ Transferencia Exitosa</h2>
                
                <div style={{ marginTop: '20px', lineHeight: '1.8' }}>
                  <p><strong>Destinatario:</strong> {comprobante.destino}</p>
                  <p><strong>Monto Enviado:</strong> $ {formatMoneda(comprobante.montoOriginal)}</p>
                </div>

                <div style={{ marginTop: '20px', padding: '15px', backgroundColor: '#1e293b', borderRadius: '8px' }}>
                  <h4 style={{ marginBottom: '10px', color: '#94a3b8' }}>Trazabilidad y Red</h4>
                  <p><strong>ID Operación (DB2):</strong> <span style={{ fontFamily: 'monospace', color: '#cbd5e1' }}>{comprobante.auditoria.id_operacion}</span></p>
                  <p><strong>ID COELSA:</strong> <span style={{ fontFamily: 'monospace', color: '#cbd5e1' }}>{comprobante.auditoria.id_coelsa}</span></p>
                  <p><strong>CAE AFIP:</strong> <span style={{ fontFamily: 'monospace', color: '#cbd5e1' }}>{comprobante.auditoria.factura_afip}</span></p>
                </div>

                {comprobante.desglose && comprobante.desglose.comision_fintech > 0 && (
                  <div style={{ marginTop: '20px', padding: '15px', backgroundColor: '#1e293b', borderRadius: '8px' }}>
                    <h4 style={{ marginBottom: '10px', color: '#94a3b8' }}>Desglose de Cargos</h4>
                    <p><strong>Comisión por Servicio:</strong> $ {formatMoneda(comprobante.desglose.comision_fintech)}</p>
                    <p><strong>IVA (21%):</strong> $ {formatMoneda(comprobante.desglose.iva_recaudado)}</p>
                  </div>
                )}

                <button 
                  onClick={() => resetearVista('TRANSF')} 
                  className="primary-btn" 
                  style={{ marginTop: '30px', backgroundColor: '#475569' }}
                >
                  Hacer otra transferencia
                </button>
              </div>
            )}

            {modoActivo === 'HIST' && (
              <div className="table-panel">
                <h2>Historial de Movimientos</h2>
                
                <div className="filters-bar">
                  <div style={{ flex: 1 }}>
                    <label>Desde Fecha:</label>
                    <input type="date" value={fechaDesde} onChange={(e) => setFechaDesde(e.target.value)} />
                  </div>
                  <div style={{ flex: 1 }}>
                    <label>Hasta Fecha:</label>
                    <input type="date" value={fechaHasta} onChange={(e) => setFechaHasta(e.target.value)} />
                  </div>
                  <button onClick={() => cargarHistorial(1)} className="primary-btn filter-btn">Filtrar</button>
                </div>

                {historial.length === 0 ? (
                  <p className="no-data">No se registran movimientos en este período.</p>
                ) : (
                  <>
                    <table className="data-table">
                      <thead>
                        <tr>
                          <th>Fecha</th>
                          <th>Descripción</th>
                          <th style={{textAlign: 'right'}}>Importe</th>
                        </tr>
                      </thead>
                      <tbody>
                        {historial.map((mov, index) => (
                          <tr key={index}>
                            <td>{mov.fecha}</td>
                            <td>{traducirOperacion(mov.tipo, mov.rel)}</td>
                            <td style={{ textAlign: 'right', fontWeight: 'bold', color: getColorMonto(mov.tipo, mov.rel) }}>
                              {getColorMonto(mov.tipo, mov.rel) === '#4ade80' ? '+' : '-'} $ {formatMoneda(mov.monto)}
                            </td>
                          </tr>
                        ))}
                      </tbody>
                    </table>
                    
                    <div className="pagination-bar">
                      <button 
                        onClick={() => cargarHistorial(pagina - 1)} 
                        disabled={pagina === 1}
                        className={pagina === 1 ? 'page-btn-disabled' : 'page-btn'}
                      >
                        ◀ Anterior
                      </button>
                      <span className="page-indicator">Página {pagina}</span>
                      <button 
                        onClick={() => cargarHistorial(pagina + 1)} 
                        disabled={!hayMas}
                        className={!hayMas ? 'page-btn-disabled' : 'page-btn'}
                      >
                        Siguiente ▶
                      </button>
                    </div>
                  </>
                )}
              </div>
            )}
          </div>
        </main>
      </div>
    );
  }

  return (
    <div className="login-layout">
      <div className="login-card">
        <div className="login-header">
          <span className="logo-icon-large">🏦</span>
          <h2>CoreBank</h2>
          <p>Portal de Acceso IBM i</p>
        </div>
        <form onSubmit={handleLogin} className="login-form">
          <div className="input-group">
            <input type="text" placeholder="Usuario" value={usuario} onChange={(e) => setUsuario(e.target.value)} required />
          </div>
          <div className="input-group">
            <input type="password" placeholder="Contraseña" value={password} onChange={(e) => setPassword(e.target.value)} required />
          </div>
          <button type="submit" className="primary-btn">Ingresar al Sistema</button>
        </form>
        {mensaje && <div className="error-message">{mensaje}</div>}
      </div>
    </div>
  );
}

export default App;