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
    log: `Double ${x} to get ${x * 2};`
  }
}

function reduceTen(x: number): OperationWithLog {
  return {
    result: x - 10,
    log: `Reduced 10 from ${x} to get ${x - 10};`
  }
}

function runWithLogs(
  value: number | ComputationWithLogs, 
  operation: (_: number): OperationWithLog
): ComputationWithLogs {
  const operationResult = 

  return {
    result: operation(),
    logs: 
  }
}