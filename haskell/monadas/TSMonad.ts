interface OperationWithLog {
  result: number;
  log: string;
}

interface ComputationWithLogs {
  result: number;
  logs: string[]
}

export function square(x: number): OperationWithLog {
  return {
    result: x * x,
    log: `Squared ${x} to get ${x * x};`
  }
}

export function double(x: number): OperationWithLog {
  return {
    result: x * 2,
    log: `Double ${x} to get ${x * 2};`
  }
}

export function reduceTen(x: number): OperationWithLog {
  return {
    result: x - 10,
    log: `Reduced 10 from ${x} to get ${x - 10};`
  }
}

export function runWithLogs(
  value: number | ComputationWithLogs, 
  operation: (_: number) => OperationWithLog
): ComputationWithLogs {
  const operationResult = typeof value === "number"
    ? operation(value)
    : operation(value.result)
  
  const logsResult = typeof value === "number"
    ? [operationResult.log]
    : [...value.logs, operationResult.log]

  return {
    result: operationResult.result,
    logs: logsResult
  }
}