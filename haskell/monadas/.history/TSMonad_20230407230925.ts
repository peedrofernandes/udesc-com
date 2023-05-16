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
    log: `Squared ${x} to get ${x * x0}`
  }
}