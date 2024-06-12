const config = {
    preset: 'ts-jest',
    testEnvironment: 'node',
    testMatch: ['<rootDir>/test/**/*.(spec|test).ts'], // matches .ts files in the 'test' directory ending with .spec.ts or .test.ts
};

export default config