import { mConStr3 } from '@meshsdk/core';
import {
    findRequests,
    getCagingScript} from '../common';
import { Context } from '../context';

const guessingLowCost = {
    mem: 1_000_000,
    steps: 1_000_000_000
};

export async function retract(
    context: Context,
    walletIndex: number,
    requestOutputRef: { txHash: string; outputIndex: number }
) : Promise<string> {
    const { log, wallet, signTx, submitTx, newTxBuilder } = context;

    const { utxos, walletAddress, collateral, signerHash } = await wallet(
        walletIndex
    );

    const { address: cageAddress, cbor: cageCbor } = getCagingScript(context);

    const requests = await findRequests(context);
    const request = requests.find(
        request =>
            request.txHash === requestOutputRef.txHash &&
            request.outputIndex === requestOutputRef.outputIndex
    );
    if (!request) {
        throw new Error('Request not found');
    }

    const { owner } = request;
    if (!request) {
        throw new Error('Request not found');
    }
    if (owner !== signerHash) {
        throw new Error('Request owner does not match signer');
    }

    const tx = newTxBuilder(); // Initialize the transaction builder
    tx.spendingPlutusScriptV3()
        .txIn(request.txHash, request.outputIndex)
        .txInInlineDatumPresent()
        .txInRedeemerValue(mConStr3([]), 'Mesh', guessingLowCost)
        .txInScript(cageCbor);

    tx.requiredSignerHash(signerHash)
        .changeAddress(walletAddress)
        .txInCollateral(collateral.input.txHash, collateral.input.outputIndex);

    await tx.complete();
    const signedTx = await signTx(walletIndex, tx);
    const txHash = await submitTx(walletIndex, signedTx);
    log('txHash', txHash);
    const block = await context.waitSettlement(txHash);
    log('block', block);
    return txHash;
}
