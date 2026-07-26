const simularArca = async (montoComision) => {
  return new Promise((resolve) => {
    const latencia = Math.floor(Math.random() * 700) + 500;
    setTimeout(() => {
      const caeAfip = '73' + Math.floor(Math.random() * 1000000000000).toString().padStart(12, '0');
      resolve(caeAfip);
    }, latencia);
  });
};

module.exports = { simularArca };