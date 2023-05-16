interface OperationWithLog {
  result: number;
  log: string;
}

interface ComputationWithLogs {
  result: number;
  logs: string[]
}

function square(x: number): OperationWithLog {
  return {
    result: x * x,
    log: `Squared ${x} to get ${x * x};`
  }
}

function double(x: number): OperationWithLog {
  return {
    result: x * 2,
    log: `Double ${x} to get ${x * 2}`
  }
}

function reduce